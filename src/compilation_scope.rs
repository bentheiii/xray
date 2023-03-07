use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::io::Write;

use crate::builtin::optional::XOptionalType;
use crate::builtin::sequence::XSequenceType;
use crate::compile_err::{CompilationError, CompilationItemCategory};
use crate::root_compilation_scope::Declaration;
use crate::units::ScopeDepth;
use crate::util::ipush::IPush;
use crate::util::special_prefix_interner::SpecialPrefixSymbol;
use crate::xexpr::XStaticFunction;
use crate::xexpr::{OverloadSpecializationBorrowed, StaticUserFunction, XExpr, XStaticExpr};
use crate::xtype::{
    common_type, Bind, CompoundKind, XCompoundSpec, XFuncSpec, XType, X_BOOL, X_FLOAT, X_INT,
    X_STRING,
};
use crate::xvalue::{DynBind, DynEvalCallback, XFunctionFactoryOutput};
use crate::Identifier;
use derivative::Derivative;
use itertools::{ExactlyOneError, Itertools};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// this is the information stored for a cell during compilation
#[derive(Derivative)]
// todo do we need to clone this? if not then we can make DynEvalCallback a Box
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub(crate) enum Cell {
    Recourse,
    /// this can also be a parameter
    Variable { t: Arc<XType>, forward_requirements: Vec<ForwardRefRequirement> },
    /// will never lead to another capture
    /// ancestor depth will never be zero
    Capture {
        ancestor_depth: ScopeDepth,
        cell_idx: usize,
    },
}

impl From<Cell> for CellSpec {
    fn from(x: Cell) -> Self {
        match x {
            Cell::Recourse => Self::Recourse,
            Cell::Variable { .. } => Self::Variable,
            Cell::Capture {
                ancestor_depth,
                cell_idx,
            } => Self::Capture {
                ancestor_depth,
                cell_idx,
            },
        }
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Copy)]
pub(crate) struct ForwardRefRequirement {
    ancestor_height: ScopeDepth,
    ref_idx: usize,
}

/// this is the information stored for a cell for it to work during runtime
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum CellSpec {
    Recourse,
    /// when the function is called, it is the caller's responsibility to fill the
    /// parameter cells with the arguments (including defaults)
    Variable,
    Capture {
        // todo i'm pretty sure this is always 1
        ancestor_depth: ScopeDepth,
        cell_idx: usize,
    },
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub(crate) enum Overload<W> {
    /// will lead to one of:
    ///  * Cell::Recourse
    ///  * Variable
    Static {
        cell_idx: usize,
        spec: XFuncSpec,
        forward_requirements: Vec<ForwardRefRequirement>,
    },
    Factory(&'static str, DynBind<W>),
}

pub(crate) struct ForwardRef {
    pub(crate) name: Identifier,
    spec: XFuncSpec,
    cell_idx: usize,
    pub(crate) fulfilled: bool,
}

pub struct CompilationScope<'p, W> {
    /// in general: the cells of a scope are organized thus:
    /// * params first
    /// * the recursion value, if any
    /// * all the variables, factory made functions
    pub(crate) cells: IPush<Cell>,
    pub(crate) declarations: Vec<Declaration<W>>,
    forwards: Vec<ForwardRef>,
    forward_requirements: HashSet<ForwardRefRequirement>,

    /// name to cell
    variables: HashMap<Identifier, usize>,
    /// name to overload
    functions: HashMap<Identifier, Vec<Overload<W>>>,
    /// name to type
    types: HashMap<Identifier, Arc<XType>>,

    parent: Option<&'p CompilationScope<'p, W>>,
    // will be none for root or lambda
    recourse_xtype: Option<Arc<XType>>,
    height: ScopeDepth,
    pub(crate) id: usize,
}

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

fn next_id() -> usize {
    NEXT_ID.fetch_add(1, Ordering::SeqCst)
}

impl<'p, W: Write + 'static> CompilationScope<'p, W> {
    pub(crate) fn root() -> Self {
        Self {
            cells: Default::default(),
            declarations: Default::default(),
            forwards: Default::default(),
            forward_requirements: Default::default(),

            variables: Default::default(),
            functions: Default::default(),
            types: Default::default(),

            parent: None,
            recourse_xtype: None,
            height: ScopeDepth(0),
            id: next_id(),
        }
    }

    pub(crate) fn from_parent_lambda(
        parent: &'p CompilationScope<'p, W>,
        parameters: impl IntoIterator<Item=(Identifier, Arc<XType>)>,
    ) -> Result<Self, CompilationError> {
        let mut ret = Self {
            parent: Some(parent),
            height: parent.height + 1,

            ..Self::root()
        };
        for (arg_idx, (param_name, param_type)) in parameters.into_iter().enumerate() {
            ret.add_parameter(param_name, arg_idx, param_type)?;
        }
        Ok(ret)
    }

    pub(crate) fn from_parent(
        parent: &'p CompilationScope<'p, W>,
        parameter_names: impl IntoIterator<Item=Identifier>,
        recourse_name: Identifier,
        recourse_spec: XFuncSpec,
    ) -> Result<Self, CompilationError> {
        let mut ret = Self::from_parent_lambda(
            parent,
            parameter_names
                .into_iter()
                .zip(recourse_spec.params.iter().map(|p| p.type_.clone())),
        )?;
        ret.add_recourse(recourse_name, recourse_spec)?;
        Ok(ret)
    }

    fn add_recourse(&mut self, name: Identifier, spec: XFuncSpec) -> Result<(), CompilationError> {
        if self._has_variable(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Overload,
            });
        }
        let cell_idx = self.cells.ipush(Cell::Recourse);
        self.recourse_xtype = Some(spec.xtype());
        self.functions
            .entry(name)
            .or_default()
            .push(Overload::Static { cell_idx, spec, forward_requirements: Default::default() });
        Ok(())
    }

    pub(crate) fn add_static_func(
        &mut self,
        name: Identifier,
        spec: XFuncSpec,
        func: XStaticFunction<W>,
    ) -> Result<(), CompilationError> {
        if self._has_variable(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Overload,
            });
        }
        let forward_requirements: Vec<_> = if let XStaticFunction::UserFunction(func) = &func {
            func.forward_requirements.iter().cloned().collect()
        } else {
            Default::default()
        };

        let cell_idx = if let Some(fref) = self.forwards.iter_mut().find(|f| !f.fulfilled && f.name == name && f.spec == spec) {
            // todo check what happens if the fulfillment has a reference as well
            fref.fulfilled = true;
            fref.cell_idx
        } else {
            let cell_idx = self.cells.ipush(Cell::Variable { t: spec.xtype(), forward_requirements: forward_requirements.clone() });
            // todo we put this twice can we avoid that?
            self.functions.entry(name).or_default().push(Overload::Static { cell_idx, spec: spec.clone(), forward_requirements });
            cell_idx
        };

        self.declarations.push(Declaration::Function { cell_idx, func });
        Ok(())
    }

    pub(crate) fn add_anonymous_func(
        &mut self,
        spec: XFuncSpec,
        func: XStaticFunction<W>,
    ) -> Result<XExpr<W>, CompilationError> {
        let cell_idx = self.cells.ipush(Cell::Variable { t: spec.xtype(), forward_requirements: Default::default() });
        self.declarations
            .push(Declaration::Function { cell_idx, func });
        Ok(XExpr::Value(cell_idx))
    }

    pub(crate) fn add_dynamic_func(
        &mut self,
        name: Identifier,
        description: &'static str,
        func: impl Fn(
            Option<&[XExpr<W>]>,
            Option<&[Arc<XType>]>,
            &mut CompilationScope<'_, W>,
            Option<&[Arc<XType>]>,
        ) -> Result<XFunctionFactoryOutput<W>, String>
        + 'static,
    ) -> Result<(), CompilationError> {
        if self._has_variable(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Overload,
            });
        }
        self.functions
            .entry(name)
            .or_default()
            .push(Overload::Factory(description, Rc::new(func)));
        Ok(())
    }

    pub(crate) fn add_variable(
        &mut self,
        name: Identifier,
        expr: XExpr<W>,
        xtype: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if self._has_overloads(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        let cell_idx = self.cells.ipush(Cell::Variable { t: xtype, forward_requirements: Default::default() });
        self.variables.insert(name, cell_idx);
        self.declarations
            .push(Declaration::Value { cell_idx, expr });
        Ok(())
    }

    fn add_parameter(
        &mut self,
        name: Identifier,
        arg_idx: usize,
        xtype: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if self._has_overloads(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        let cell_idx = self.cells.ipush(Cell::Variable { t: xtype, forward_requirements: Default::default() });
        self.variables.insert(name, cell_idx);
        self.declarations.push(Declaration::Parameter {
            cell_idx,
            argument_idx: arg_idx,
        });
        Ok(())
    }

    pub(crate) fn add_type(
        &mut self,
        name: Identifier,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if self._has_overloads(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Type,
            });
        }
        if self._has_variable(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Type,
            });
        }
        self.types.insert(name, type_);
        Ok(())
    }

    pub(crate) fn add_compound(
        &mut self,
        name: Identifier,
        kind: CompoundKind,
        struct_spec: XCompoundSpec,
    ) -> Result<(), CompilationError> {
        if self._has_overloads(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Type,
            });
        }
        if self._has_variable(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Type,
            });
        }
        self.types.insert(
            name,
            Arc::new(XType::Compound(
                kind,
                Arc::new(struct_spec),
                Bind::default(),
            )),
        );
        Ok(())
    }

    pub(crate) fn add_forward_func(
        &mut self,
        name: Identifier,
        spec: XFuncSpec,
    ) -> Result<(), CompilationError> {
        if self._has_variable(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self._has_type(&name) {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Overload,
            });
        }
        let requirement = ForwardRefRequirement { ancestor_height: self.height, ref_idx: self.forwards.len() };
        let cell_idx = self.cells.ipush(Cell::Variable { t: spec.xtype(), forward_requirements: vec![requirement] });
        let fref = ForwardRef { name, spec: spec.clone(), cell_idx, fulfilled: false };
        self.forwards.push(fref);
        self.functions
            .entry(name)
            .or_default()
            // todo we currently store the requirements twice, can we avoid that?
            .push(Overload::Static { cell_idx, spec, forward_requirements: vec![requirement] });
        Ok(())
    }

    pub(crate) fn forward_ref(&self, freq: &ForwardRefRequirement)->&ForwardRef{
        let depth = self.height - freq.ancestor_height;
        &self.ancestor_at_depth(depth).forwards[freq.ref_idx]
    }

    fn require_forwards(&mut self, refs: impl IntoIterator<Item=ForwardRefRequirement>) -> Result<(), CompilationError> {
        for freq in refs {
            let fref = &self.forward_ref(&freq);
            if !fref.fulfilled {
                if freq.ancestor_height == self.height {
                    return Err(CompilationError::MissingForwardImplementation { name: fref.name, spec: fref.spec.clone() });
                } else {
                    self.forward_requirements.insert(freq);
                }
            }
        }
        Ok(())
    }

    pub(crate) fn into_static_ud(
        self,
        name: Option<String>,
        defaults: Vec<XExpr<W>>,
        param_len: usize,
        output: Box<XExpr<W>>,
        parent_id: usize,
    ) -> (StaticUserFunction<W>, Vec<Cell>) {
        /*
        what this function returns is an ugly patch
        basically the problem is we have no way to tell a parent a scope to capture something
        for the child scope while the child scope is still alive, so we return a list of cells we
        want the parent to capture
        TODO a possible way to improve this is to let the child scope keep track of its requests
         as it is building them, and then to integrate them back into the parent when the child is
         disposed
         */

        let mut parent_capture_requests = Vec::new();
        let mut cell_specs = Vec::new();
        if let Some(parent) = self.parent {
            let mut parent_new_cell_idx = parent.cells.len();
            for cell in self.cells {
                let spec = if let Cell::Capture {
                    ancestor_depth,
                    cell_idx,
                } = cell
                {
                    if ancestor_depth.0 > 1 {
                        parent_capture_requests.push(Cell::Capture {
                            ancestor_depth: ancestor_depth - 1,
                            cell_idx,
                        });
                        parent_new_cell_idx += 1;
                        CellSpec::Capture {
                            ancestor_depth: ScopeDepth(1),
                            cell_idx: parent_new_cell_idx - 1,
                        }
                    } else {
                        CellSpec::from(cell)
                    }
                } else {
                    CellSpec::from(cell)
                };
                cell_specs.push(spec)
            }
        } else {
            cell_specs.extend(self.cells.into_iter().map(CellSpec::from))
        }
        (
            StaticUserFunction {
                name,
                defaults,
                param_len,
                cell_specs,
                declarations: self.declarations,
                output,
                parent_id,
                id: self.id,
                forward_requirements: self.forward_requirements,
            },
            parent_capture_requests,
        )
    }

    pub(crate) fn get_item(&self, name: &Identifier) -> Option<CompilationItem<W>> {
        if let Some(&cell_idx) = self.variables.get(name) {
            let Cell::Variable { forward_requirements, .. } = &self.cells[cell_idx] else { unreachable!() };
            Some(CompilationItem::Value((self.height, cell_idx, forward_requirements.clone())))
        } else if let Some(t) = self.types.get(name) {
            Some(CompilationItem::Type(t.clone()))
        } else if let Some(overloads) = self.functions.get(name) {
            let my_height = self.height;
            let mut ret: Vec<_> = overloads.iter().map(|ov| (my_height, ov.clone())).collect();
            if let Some(CompilationItem::Overload(parent_overloads)) =
                self.parent.and_then(|p| p.get_item(name))
            {
                if let Some(recourse_xtype) = self.recourse_xtype.clone(){
                    // we need to discard forward references that are fulfilled by the current scope
                    for overload in parent_overloads{
                        if let Overload::Static {forward_requirements, spec, ..} = &overload.1{
                            if spec.xtype() == recourse_xtype && forward_requirements.iter().any(|freq| !self.forward_ref(freq).fulfilled){
                                continue
                            }
                        }
                        ret.push(overload)
                    }
                } else {
                    ret.extend(parent_overloads)
                }
            }
            Some(CompilationItem::Overload(ret))
        } else if let Some(parent) = self.parent {
            parent.get_item(name)
        } else {
            None
        }
    }

    fn _has_overloads(&self, name: &Identifier) -> bool {
        self.functions.contains_key(name)
    }

    fn _has_variable(&self, name: &Identifier) -> bool {
        self.variables.contains_key(name)
    }

    fn _has_type(&self, name: &Identifier) -> bool {
        self.types.contains_key(name)
    }

    pub(crate) fn compile(
        &mut self,
        stat_expr: XStaticExpr<W>,
    ) -> Result<XExpr<W>, CompilationError> {
        match stat_expr {
            XStaticExpr::LiteralBool(v) => Ok(XExpr::LiteralBool(v)),
            XStaticExpr::LiteralInt(v) => Ok(XExpr::LiteralInt(v)),
            XStaticExpr::LiteralFloat(v) => Ok(XExpr::LiteralFloat(v)),
            XStaticExpr::LiteralString(v) => Ok(XExpr::LiteralString(v)),
            XStaticExpr::Array(items) => Ok(XExpr::Array(
                items
                    .into_iter()
                    .map(|i| self.compile(i))
                    .collect::<Result<_, _>>()?,
            )),
            XStaticExpr::Member(obj, member_name) => {
                let obj = self.compile(*obj)?;
                let obj_type = self.type_of(&obj)?;
                let member_idx = match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Struct, spec, _) => spec.find(member_name)?,
                    XType::Compound(..) => {
                        return Err(CompilationError::NonVariantMemberAccess { xtype: obj_type });
                    }
                    XType::Tuple(types) => {
                        let SpecialPrefixSymbol::Item(idx) = member_name else {
                            return Err(CompilationError::NonItemTupleAccess {
                                member: member_name,
                            });
                        };
                        if idx >= types.len() {
                            return Err(CompilationError::TupleIndexOutOfBounds {
                                tuple_type: obj_type.clone(),
                                index: idx,
                                max: types.len(),
                            });
                        } else {
                            idx
                        }
                    }
                    _ => return Err(CompilationError::NonCompoundMemberAccess { xtype: obj_type }),
                };
                Ok(XExpr::Member(Box::new(obj), member_idx))
            }
            XStaticExpr::MemberValue(obj, member_name) => {
                let obj = self.compile(*obj)?;
                let obj_type = self.type_of(&obj)?;
                let member_idx = match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Union, spec, _) => spec.find(member_name)?,
                    XType::Tuple(..) | XType::Compound(..) => {
                        return Err(CompilationError::NonUnionVariantAccess { xtype: obj_type });
                    }
                    _ => return Err(CompilationError::NonCompoundMemberAccess { xtype: obj_type }),
                };
                Ok(XExpr::MemberValue(Box::new(obj), member_idx))
            }
            XStaticExpr::MemberOptValue(obj, member_name) => {
                let obj = self.compile(*obj)?;
                let obj_type = self.type_of(&obj)?;
                let member_idx = match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Union, spec, _) => spec.find(member_name)?,
                    XType::Tuple(..) | XType::Compound(..) => {
                        return Err(CompilationError::NonUnionVariantAccess { xtype: obj_type });
                    }
                    _ => return Err(CompilationError::NonCompoundMemberAccess { xtype: obj_type }),
                };
                Ok(XExpr::MemberOptValue(Box::new(obj), member_idx))
            }
            XStaticExpr::Tuple(items) => Ok(XExpr::Tuple(
                items
                    .into_iter()
                    .map(|i| self.compile(i))
                    .collect::<Result<_, _>>()?,
            )),
            XStaticExpr::Lambda(spec, func) => self.add_anonymous_func(spec, *func),
            XStaticExpr::SpecializedIdent(name, specialization) => match self.get_item(&name) {
                Some(CompilationItem::Overload(overloads)) => {
                    self.resolve_overload(overloads, None, specialization.borrow(), name)
                }
                Some(CompilationItem::Type(t)) => {
                    Err(CompilationError::SpecializationOfType { name, type_: t })
                }
                Some(CompilationItem::Value(..)) => {
                    Err(CompilationError::SpecializationOfVariable { name })
                }
                None => Err(CompilationError::ValueNotFound { name }),
            },
            XStaticExpr::Ident(name) => {
                let (height, cell_idx, forward_requirements) = match self.get_item(&name) {
                    Some(CompilationItem::Value(trace)) => trace,
                    Some(CompilationItem::Overload(overloads)) => {
                        if overloads.len() > 1 {
                            return Err(CompilationError::OverloadedFunctionAsVariable { name });
                        }
                        if let (height, Overload::Static { cell_idx, forward_requirements, .. }) = &overloads[0] {
                            (*height, *cell_idx, forward_requirements.clone())
                        } else {
                            // this happens if the function is dynamic
                            return Err(CompilationError::OverloadedFunctionAsVariable { name });
                        }
                    }
                    Some(CompilationItem::Type(t)) => {
                        return Err(CompilationError::TypeAsVariable { type_: t });
                    }
                    None => return Err(CompilationError::ValueNotFound { name }),
                };
                let new_cell_idx = if height == self.height {
                    cell_idx
                } else {
                    self.require_forwards(forward_requirements)?;
                    let new_cell = Cell::Capture {
                        ancestor_depth: self.height - height,
                        cell_idx,
                    };
                    self.cells.ipush(new_cell)
                };
                Ok(XExpr::Value(new_cell_idx))
            }
            XStaticExpr::Call(expr, args) => {
                let args: Vec<XExpr<W>> = args
                    .into_iter()
                    .map(|a| self.compile(a))
                    .collect::<Result<_, _>>()?;
                // handle special cases
                match expr.as_ref() {
                    XStaticExpr::Member(obj, member_name) => {
                        // special case: union variant constructor
                        if let XStaticExpr::Ident(name) = obj.as_ref() {
                            if let Some(CompilationItem::Type(t)) = self.get_item(name) {
                                if let XType::Compound(CompoundKind::Union, spec, ..) = t.as_ref() {
                                    return if let Some(&index) = spec.indices.get(member_name) {
                                        if args.len() != 1 {
                                            return Err(CompilationError::VariantConstructorOneArg);
                                        }
                                        let compiled_arg = args.into_iter().next().unwrap();
                                        let com_type = Arc::new(XType::Compound(
                                            CompoundKind::Union,
                                            spec.clone(),
                                            Bind::new(),
                                        ));
                                        let var_type = spec.fields[index]
                                            .type_
                                            .resolve_bind(&Bind::new(), Some(&com_type));
                                        if let Some(bind) = var_type
                                            .bind_in_assignment(&self.type_of(&compiled_arg)?)
                                        {
                                            return Ok(XExpr::Variant(
                                                spec.clone(),
                                                bind,
                                                index,
                                                Box::new(compiled_arg),
                                            ));
                                        } else {
                                            Err(CompilationError::VariantConstructorTypeArgMismatch {
                                                union_name: spec.name,
                                                variant_name: *member_name,
                                                expected_type: spec.fields[index].type_.clone(),
                                                actual_type: self.type_of(&compiled_arg)?,
                                            })
                                        }
                                    } else {
                                        Err(CompilationError::MemberNotFound {
                                            spec: spec.clone(),
                                            name: *member_name,
                                        })
                                    };
                                }
                            }
                        }
                    }
                    XStaticExpr::Ident(name) => {
                        match self.get_item(name) {
                            Some(CompilationItem::Overload(overloads)) => {
                                // special case: overloaded function
                                let arg_types: Vec<_> = args
                                    .iter()
                                    .map(|a| self.type_of(a))
                                    .collect::<Result<_, _>>()?;
                                let overload = self.resolve_overload(
                                    overloads,
                                    Some(&args),
                                    OverloadSpecializationBorrowed::ParamTypes(arg_types.borrow()),
                                    *name,
                                )?;
                                return Ok(XExpr::Call(Box::new(overload), args));
                            }
                            Some(CompilationItem::Type(struct_t)) => {
                                // special case: struct construction
                                if let XType::Compound(CompoundKind::Struct, spec, binding) =
                                    struct_t.as_ref()
                                {
                                    let arg_types: Vec<_> = args
                                        .iter()
                                        .map(|a| self.type_of(a))
                                        .collect::<Result<_, _>>()?;
                                    if arg_types.len() != spec.fields.len() {
                                        return Err(CompilationError::StructParamsLengthMismatch {
                                            struct_name: spec.name,
                                            expected_count: spec.fields.len(),
                                            actual_count: arg_types.len(),
                                        });
                                    };
                                    return if let Some(bind) =
                                        spec.bind(&arg_types[..], &struct_t, binding)
                                    {
                                        Ok(XExpr::Construct(spec.clone(), bind, args))
                                    } else {
                                        Err(CompilationError::StructFieldTypeMismatch {
                                            struct_name: spec.name,
                                            expected_types: spec
                                                .fields
                                                .iter()
                                                .map(|x| x.type_.clone())
                                                .collect(),
                                            actual_types: arg_types,
                                        })
                                    };
                                }
                            }
                            _ => {}
                        }
                    }
                    XStaticExpr::SpecializedIdent(name, specialization) => {
                        // special case, call to specialized function
                        if let Some(CompilationItem::Overload(overloads)) = self.get_item(name) {
                            let overload = self.resolve_overload(
                                overloads,
                                Some(&args),
                                specialization.borrow(),
                                *name,
                            )?;
                            return Ok(XExpr::Call(Box::new(overload), args));
                        }
                    }
                    _ => {}
                }
                let callee = self.compile(*expr)?;
                Ok(XExpr::Call(Box::new(callee), args))
            }
        }
    }

    pub(crate) fn type_of(&self, expr: &XExpr<W>) -> Result<Arc<XType>, CompilationError> {
        match expr {
            XExpr::LiteralBool(..) => Ok(X_BOOL.clone()),
            XExpr::LiteralInt(..) => Ok(X_INT.clone()),
            XExpr::LiteralFloat(..) => Ok(X_FLOAT.clone()),
            XExpr::LiteralString(..) => Ok(X_STRING.clone()),
            XExpr::Array(exprs) => {
                let element_type = common_type(exprs.iter().map(|x| self.type_of(x)))?;
                Ok(XType::XNative(Box::new(XSequenceType), vec![element_type]).into())
            }
            XExpr::Call(func0, args) => {
                let func_type = self.type_of(func0)?;
                if let XType::XCallable(spec) = func_type.as_ref() {
                    return Ok(spec.return_type.clone());
                }
                if let XType::XFunc(func) = func_type.as_ref() {
                    let mut bind = Bind::new();
                    for (param, arg) in func.params.iter().zip(args) {
                        let arg_type = self.type_of(arg)?;
                        bind = bind
                            .mix(&param.type_.bind_in_assignment(&arg_type).ok_or(
                                CompilationError::InvalidArgumentType {
                                    expected: param.type_.clone(),
                                    got: arg_type,
                                },
                            )?)
                            .ok_or(CompilationError::CallableBindingFailed)?;
                    }
                    return Ok(func.rtype(&bind));
                }
                Err(CompilationError::NotAFunction { type_: func_type })
            }
            XExpr::Construct(spec, binding, ..) => Ok(Arc::new(XType::Compound(
                CompoundKind::Struct,
                spec.clone(),
                binding.clone(),
            ))),
            XExpr::Variant(spec, binding, ..) => Ok(Arc::new(XType::Compound(
                CompoundKind::Union,
                spec.clone(),
                binding.clone(),
            ))),
            XExpr::Member(obj, idx) => {
                let obj_type = self.type_of(obj)?;
                match obj_type.as_ref() {
                    XType::Compound(ck, spec, bind) => {
                        assert_eq!(*ck, CompoundKind::Struct);
                        Ok(spec.fields[*idx]
                            .type_
                            .clone()
                            .resolve_bind(bind, Some(&obj_type)))
                    }
                    XType::Tuple(fields) => Ok(fields[*idx].clone()),
                    _ => Err(CompilationError::NonCompoundMemberAccess {
                        xtype: obj_type.clone(),
                    }),
                }
            }
            XExpr::MemberValue(obj, idx) => {
                let obj_type = self.type_of(obj)?;
                match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Union, spec, bind) => Ok(spec.fields[*idx]
                        .type_
                        .clone()
                        .resolve_bind(bind, Some(&obj_type))),
                    XType::Compound(CompoundKind::Struct, ..) | XType::Tuple(..) => {
                        Err(CompilationError::NonUnionVariantAccess {
                            xtype: obj_type.clone(),
                        })
                    }
                    _ => Err(CompilationError::NonCompoundMemberAccess {
                        xtype: obj_type.clone(),
                    }),
                }
            }
            XExpr::MemberOptValue(obj, idx) => {
                let obj_type = self.type_of(obj)?;
                match obj_type.as_ref() {
                    XType::Compound(CompoundKind::Union, spec, bind) => {
                        let t = spec.fields[*idx]
                            .type_
                            .clone()
                            .resolve_bind(bind, Some(&obj_type));
                        Ok(XOptionalType::xtype(t))
                    }
                    XType::Compound(CompoundKind::Struct, ..) | XType::Tuple(..) => {
                        Err(CompilationError::NonUnionVariantAccess {
                            xtype: obj_type.clone(),
                        })
                    }
                    _ => Err(CompilationError::NonCompoundMemberAccess {
                        xtype: obj_type.clone(),
                    }),
                }
            }
            XExpr::Tuple(items) => {
                let types = items
                    .iter()
                    .map(|x| self.type_of(x))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Arc::new(XType::Tuple(types)))
            }
            XExpr::Value(mut cell_idx) => {
                let mut scope = self;
                loop {
                    let cell = &scope.cells[cell_idx];
                    match cell {
                        Cell::Variable { t, .. } => break Ok(t.clone()),
                        Cell::Capture {
                            ancestor_depth,
                            cell_idx: new_idx,
                        } => {
                            scope = scope.ancestor_at_depth(*ancestor_depth);
                            cell_idx = *new_idx
                        }
                        Cell::Recourse => break Ok(scope.recourse_xtype.clone().unwrap()),
                    }
                }
            }
            XExpr::Dummy(..) => unreachable!(),
        }
    }

    pub(crate) fn get_func(
        &mut self,
        name: &Identifier,
        arg_types: &[Arc<XType>],
    ) -> Result<XExpr<W>, CompilationError> {
        let Some(CompilationItem::Overload(overloads)) = self.get_item(name) else { return Err(CompilationError::NoOverload { name: *name, param_types: Some(arg_types.to_vec()), dynamic_failures: Vec::new() }); };
        let overloads = overloads.into_iter().filter(|cand| {
            if let Overload::Static { forward_requirements, .. } = &cand.1 {
                forward_requirements.iter().all(|r|self.forward_ref(r).fulfilled)
            } else {
                true
            }
        }).collect::<Vec<_>>();
        self.resolve_overload(
            overloads,
            None,
            OverloadSpecializationBorrowed::ParamTypes(arg_types),
            *name,
        )
    }

    pub(crate) fn resolve_overload(
        &mut self,
        overloads: impl IntoIterator<Item=TracedOverload<W>>,
        args: Option<&[XExpr<W>]>,
        specialization: OverloadSpecializationBorrowed<'_>,
        name: Identifier,
    ) -> Result<XExpr<W>, CompilationError> {
        #[derive(Derivative)]
        #[derivative(Debug(bound = ""))]
        enum OverloadToConsider<W> {
            /// height, cell
            FromCell(ScopeDepth, usize, Vec<ForwardRefRequirement>),
            FromFactory(
                Arc<XType>,
                #[derivative(Debug = "ignore")] DynEvalCallback<W>,
            ),
        }
        fn prepare_return<W: Write + 'static>(
            namespace: &mut CompilationScope<W>,
            considered: OverloadToConsider<W>,
        ) -> Result<XExpr<W>, CompilationError> {
            let new_cell = match considered {
                OverloadToConsider::FromCell(height, cell, forward_reqs) => {
                    namespace.require_forwards(forward_reqs)?;
                    if height == namespace.height {
                        cell
                    } else {
                        namespace.cells.ipush(Cell::Capture {
                            ancestor_depth: namespace.height - height,
                            cell_idx: cell,
                        })
                    }
                }
                OverloadToConsider::FromFactory(t, f) => {
                    let cell_idx = namespace.cells.ipush(Cell::Variable { t, forward_requirements: Default::default() });
                    namespace
                        .declarations
                        .push(Declaration::FactoryFunction { cell_idx, cb: f });
                    cell_idx
                }
            };
            Ok(XExpr::Value(new_cell))
        }
        let arg_types = match specialization {
            OverloadSpecializationBorrowed::Binding(..) => None,
            OverloadSpecializationBorrowed::ParamTypes(at) => {
                let mut new_types = vec![];
                for (i, t) in at.iter().enumerate() {
                    new_types.push(if let XType::Auto = t.as_ref() {
                        match args {
                            None => return Err(CompilationError::AutoSpecializationWithoutCall),
                            Some(args) => self.type_of(&args[i])?,
                        }
                    } else {
                        t.clone()
                    })
                }
                Some(new_types)
            }
        };
        let dynamic_bind_types = if let OverloadSpecializationBorrowed::Binding(b) = specialization
        {
            Some(b)
        } else {
            None
        };

        let mut exact_matches = vec![];
        let mut generic_matches = vec![];
        let mut dynamic_failures = vec![];
        // if the bindings are unknown, then we prefer generic solutions over exact solutions
        let is_unknown = arg_types
            .as_ref()
            .map_or(true, |t| t.iter().any(|t| t.is_unknown()));
        for (height, overload) in overloads {
            let (spec, considered, is_generic) = match &overload {
                Overload::Static { spec, cell_idx, forward_requirements } => {
                    if dynamic_bind_types.is_some() {
                        continue;
                    }
                    (
                        spec.clone(),
                        OverloadToConsider::FromCell(height, *cell_idx, forward_requirements.clone()),
                        spec.is_generic(),
                    )
                }
                Overload::Factory(desc, dyn_func) => {
                    match dyn_func(args, arg_types.as_deref(), self, dynamic_bind_types) {
                        Ok(overload) => {
                            let xtype = overload.spec.xtype();
                            (
                                overload.spec,
                                OverloadToConsider::FromFactory(xtype, overload.func),
                                true,
                            )
                        }
                        Err(err) => {
                            dynamic_failures.push((*desc, err));
                            continue;
                        }
                    }
                }
            };
            let b = arg_types.as_ref().map_or_else(
                || Some(Default::default()),
                |arg_types| spec.bind(arg_types),
            );
            if let Some(_bind) = b {
                if spec.short_circuit_overloads {
                    return prepare_return(self, considered);
                }
                if is_generic ^ is_unknown {
                    &mut generic_matches
                } else {
                    &mut exact_matches
                }
                    .push(considered);
            }
        }
        if exact_matches.len() == 1 {
            return prepare_return(self, exact_matches.swap_remove(0));
        }
        if exact_matches.len() > 1 {
            return Err(CompilationError::AmbiguousOverload {
                name,
                is_generic: is_unknown,
                items: exact_matches.len(),
                param_types: arg_types.map(|at| at.to_vec()),
            });
        }
        if generic_matches.len() == 1 {
            return prepare_return(self, generic_matches.swap_remove(0));
        }
        if generic_matches.len() > 1 {
            return Err(CompilationError::AmbiguousOverload {
                name,
                is_generic: !is_unknown,
                items: generic_matches.len(),
                param_types: arg_types.map(|at| at.to_vec()),
            });
        }
        Err(CompilationError::NoOverload {
            name,
            param_types: arg_types.map(|at| at.to_vec()),
            dynamic_failures,
        })
    }

    pub(crate) fn get_unique_function<'a>(
        &'a self,
        name: &Identifier,
    ) -> Result<Option<&Overload<W>>, ExactlyOneError<impl Iterator + 'a>> {
        self.functions
            .get(name)
            .map_or(Ok(None), |lst| lst.iter().exactly_one().map(Some))
    }

    pub(crate) fn get_variable_cell(&self, name: &Identifier) -> Option<&usize> {
        self.variables.get(name)
    }

    fn ancestor_at_depth(&self, target_depth: ScopeDepth) -> &Self {
        let mut ret = self;
        for _ in 0..target_depth.0 {
            ret = ret.parent.unwrap();
        }
        ret
    }
}

type TracedOverload<W> = (ScopeDepth, Overload<W>);
type TracedValue = (ScopeDepth, usize, Vec<ForwardRefRequirement>);

pub(crate) enum CompilationItem<W> {
    Value(TracedValue),
    /// guaranteed to never be empty
    Overload(Vec<TracedOverload<W>>),
    Type(Arc<XType>),
}
