use std::borrow::Borrow;
use std::collections::HashMap;
use std::io::Write;

use crate::compile_err::CompilationItemCategory;
use crate::units::ScopeDepth;
use crate::util::ipush::IPush;
use crate::util::special_prefix_interner::SpecialPrefixSymbol;
use crate::xexpr::{OverloadSpecializationBorrowed, StaticUserFunction, XExpr};
use crate::xtype::{common_type, CompoundKind, X_BOOL, X_FLOAT, X_INT, X_STRING};
use crate::xvalue::{DynBind, NativeCallable, XFunctionFactoryOutput};
use crate::{
    Bind, CompilationError, Declaration, Identifier, XCompoundSpec, XFuncSpec, XOptionalType,
    XSequenceType, XStaticExpr, XStaticFunction, XType,
};
use derivative::Derivative;
use itertools::{ExactlyOneError, Itertools};
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// this is the information stored for a cell during compilation
#[derive(Derivative)]
#[derivative(Clone(bound = ""), Debug(bound = ""))]
pub(crate) enum Cell<W: Write + 'static> {
    Recourse,
    /// this can also be a parameter
    Variable(Arc<XType>),
    /// will never lead to another capture
    /// ancestor depth will never be zero
    Capture {
        ancestor_depth: ScopeDepth,
        cell_idx: usize,
    },
    FactoryMadeFunction(Arc<XType>, XStaticFunction<W>),
}

impl<W: Write + 'static> From<Cell<W>> for CellSpec<W> {
    fn from(x: Cell<W>) -> Self {
        match x {
            Cell::Recourse => Self::Recourse,
            Cell::Variable(..) => Self::Variable,
            Cell::Capture {
                ancestor_depth,
                cell_idx,
            } => Self::Capture {
                ancestor_depth,
                cell_idx,
            },
            Cell::FactoryMadeFunction(_t, func) => match func {
                XStaticFunction::Native(native) => Self::FactoryMadeFunction(native),
                XStaticFunction::UserFunction(..) => {
                    panic!("unexpected factory resulting in user function")
                }
            },
        }
    }
}

/// this is the information stored for a cell for it to work during runtime
#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum CellSpec<W: Write + 'static> {
    Recourse,
    /// when the function is called, it is the caller's responsibility to fill the
    /// parameter cells with the arguments (including defaults)
    Variable,
    Capture {
        ancestor_depth: ScopeDepth,
        cell_idx: usize,
    },
    FactoryMadeFunction(#[derivative(Debug = "ignore")] NativeCallable<W>),
}

#[derive(Derivative)]
#[derivative(Clone(bound = ""))]
pub(crate) enum Overload<W: Write + 'static> {
    /// will lead to one of:
    ///  * Cell::Recourse
    ///  * Variable
    Static {
        cell_idx: usize,
        spec: XFuncSpec,
    },
    Factory(&'static str, DynBind<W>),
}

pub struct CompilationScope<'p, W: Write + 'static> {
    /// in general: the cells of a scope are organized thus:
    /// * params first
    /// * the recursion value, if any
    /// * all the variables, factory made functions
    pub(crate) cells: IPush<Cell<W>>,
    pub(crate) declarations: Vec<Declaration<W>>,

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
        parameters: impl IntoIterator<Item = (Identifier, Arc<XType>)>,
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
        parameter_names: impl IntoIterator<Item = Identifier>,
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
        if self.get_variable(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self.get_type(&name).is_some() {
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
            .push(Overload::Static { cell_idx, spec });
        Ok(())
    }

    pub(crate) fn add_static_func(
        &mut self,
        name: Identifier,
        spec: XFuncSpec,
        func: XStaticFunction<W>,
    ) -> Result<(), CompilationError> {
        if self.get_variable(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self.get_type(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Overload,
            });
        }
        let cell_idx = self.cells.ipush(Cell::Variable(spec.xtype()));
        self.functions
            .entry(name)
            .or_default()
            .push(Overload::Static { cell_idx, spec });
        self.declarations
            .push(Declaration::Function { cell_idx, func });
        Ok(())
    }

    pub(crate) fn add_anonymous_func(
        &mut self,
        spec: XFuncSpec,
        func: XStaticFunction<W>,
    ) -> Result<XExpr<W>, CompilationError> {
        let cell_idx = self.cells.ipush(Cell::Variable(spec.xtype()));
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
        if self.get_variable(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Value,
                new_category: CompilationItemCategory::Overload,
            });
        }
        if self.get_type(&name).is_some() {
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
        if !self.get_overloads(&name).is_empty() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self.get_type(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        let cell_idx = self.cells.ipush(Cell::Variable(xtype));
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
        if !self.get_overloads(&name).is_empty() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self.get_type(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        let cell_idx = self.cells.ipush(Cell::Variable(xtype));
        self.variables.insert(name, cell_idx);
        self.declarations.push(Declaration::Parameter {
            cell_idx,
            argument_idx: arg_idx,
        });
        Ok(())
    }

    pub(crate) fn add_native_type(
        &mut self,
        name: Identifier,
        type_: Arc<XType>,
    ) -> Result<(), CompilationError> {
        if !self.get_overloads(&name).is_empty() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Type,
            });
        }
        if self.get_variable(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self.get_type(&name).is_some() {
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
        if !self.get_overloads(&name).is_empty() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Overload,
                new_category: CompilationItemCategory::Type,
            });
        }
        if self.get_variable(&name).is_some() {
            return Err(CompilationError::IllegalShadowing {
                name,
                current_category: CompilationItemCategory::Type,
                new_category: CompilationItemCategory::Value,
            });
        }
        if self.get_type(&name).is_some() {
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

    pub(crate) fn into_static_ud(
        self,
        name: Option<String>,
        defaults: Vec<XExpr<W>>,
        param_len: usize,
        output: Box<XExpr<W>>,
        parent_id: usize,
    ) -> StaticUserFunction<W> {
        StaticUserFunction {
            name,
            defaults,
            param_len,
            cell_specs: self.cells.into_iter().map(CellSpec::from).collect(),
            declarations: self.declarations,
            output,
            parent_id,
            id: self.id,
        }
    }

    fn get_overloads(&self, name: &Identifier) -> Vec<TracedOverload<W>> {
        let mut parent = self.parent.map_or_else(Vec::new, |p| p.get_overloads(name));
        if let Some(my_overloads) = self.functions.get(name) {
            let my_height = self.height;
            parent.extend(my_overloads.iter().map(move |ov| (my_height, ov.clone())))
        }
        parent
    }

    fn get_variable(&self, name: &Identifier) -> Option<TracedValue> {
        self.variables
            .get(name)
            .map(|&cell_idx| (self.height, cell_idx))
            .or_else(|| self.parent.and_then(|p| p.get_variable(name)))
    }

    pub(crate) fn get_type(&self, name: &Identifier) -> Option<Arc<XType>> {
        self.types
            .get(name)
            .cloned()
            .or_else(|| self.parent.and_then(|p| p.get_type(name)))
    }

    pub(crate) fn compile(&mut self, stat_expr: XStaticExpr) -> Result<XExpr<W>, CompilationError> {
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
                    XType::Compound(CompoundKind::Struct, spec, _) => {
                        spec.find(member_name)?
                    }
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
                    XType::Compound(CompoundKind::Union, spec, _) => {
                        spec.find(member_name)?
                    }
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
                    XType::Compound(CompoundKind::Union, spec, _) => {
                        spec.find(member_name)?
                    }
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
            XStaticExpr::Lambda(args, output) => {
                let mut subscope = CompilationScope::from_parent_lambda(
                    self,
                    args.iter().map(|arg| (arg.name, arg.type_.clone())),
                )?;
                let output = subscope.compile(*output)?;
                let output_type = subscope.type_of(&output)?;
                let param_len = args.len();
                let arg_types: Vec<_> = args.iter().map(|a| &a.type_).collect();
                let spec = XFuncSpec::new(&arg_types, output_type);
                let defaults = args
                    .into_iter()
                    .filter_map(|a| a.default.map(|s| subscope.compile(s)))
                    .collect::<Result<_, _>>()?;
                let ud_func =
                    subscope.into_static_ud(None, defaults, param_len, Box::new(output), self.id);
                self.add_anonymous_func(spec, XStaticFunction::UserFunction(Rc::new(ud_func)))
            }
            XStaticExpr::SpecializedIdent(name, specialization) => {
                let overloads = self.get_overloads(&name);
                if overloads.is_empty() {
                    return Err(if let Some(t) = self.get_type(&name) {
                        CompilationError::SpecializationOfType { name, type_: t }
                    } else if let Some(..) = self.get_variable(&name) {
                        CompilationError::SpecializationOfVariable { name }
                    } else {
                        CompilationError::ValueNotFound { name }
                    });
                }
                self.resolve_overload(overloads, None, specialization.borrow(), name)
            }
            XStaticExpr::Ident(name) => {
                let (height, cell_idx) = if let Some((height, cell_idx)) = self.get_variable(&name)
                {
                    (height, cell_idx)
                } else {
                    let mut overloads = self.get_overloads(&name);
                    if overloads.is_empty() {
                        return Err(if let Some(t) = self.get_type(&name) {
                            CompilationError::TypeAsVariable { type_: t }
                        } else {
                            CompilationError::ValueNotFound { name }
                        });
                    }
                    if overloads.len() > 1 {
                        return Err(CompilationError::OverloadedFunctionAsVariable { name });
                    }
                    if let (height, Overload::Static { cell_idx, .. }) = overloads.swap_remove(0) {
                        (height, cell_idx)
                    } else {
                        return Err(CompilationError::OverloadedFunctionAsVariable { name });
                    }
                };
                let new_cell_idx = if height == self.height {
                    cell_idx
                } else {
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
                            if let Some(XType::Compound(CompoundKind::Union, spec, ..)) =
                                self.get_type(name).as_deref()
                            {
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
                                    if let Some(bind) =
                                        var_type.bind_in_assignment(&self.type_of(&compiled_arg)?)
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
                    XStaticExpr::Ident(name) => {
                        // special case: overloaded function
                        let overloads = self.get_overloads(name);
                        if !overloads.is_empty() {
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
                        // special case: struct construction
                        if let Some(XType::Compound(CompoundKind::Struct, spec, ..)) =
                            self.get_type(name).as_deref()
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
                            return if let Some(bind) = spec.bind(&arg_types[..]) {
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
                    XStaticExpr::SpecializedIdent(name, specialization) => {
                        // special case, call to specialized function
                        let overloads = self.get_overloads(name);
                        if !overloads.is_empty() {
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
                            .mix(&param.type_.bind_in_assignment(&arg_type).unwrap())
                            .unwrap()
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
                        Cell::Variable(t) => break Ok(t.clone()),
                        Cell::Capture {
                            ancestor_depth,
                            cell_idx: new_idx,
                        } => {
                            scope = scope.ancestor_at_depth(*ancestor_depth);
                            cell_idx = *new_idx
                        }
                        Cell::Recourse => break Ok(scope.recourse_xtype.clone().unwrap()),
                        Cell::FactoryMadeFunction(t, ..) => break Ok(t.clone()),
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
        let overloads = self.get_overloads(name);
        self.resolve_overload(
            overloads,
            None,
            OverloadSpecializationBorrowed::ParamTypes(arg_types),
            *name,
        )
    }

    pub(crate) fn resolve_overload(
        &mut self,
        overloads: impl IntoIterator<Item = TracedOverload<W>>,
        args: Option<&[XExpr<W>]>,
        specialization: OverloadSpecializationBorrowed<'_>,
        name: Identifier,
    ) -> Result<XExpr<W>, CompilationError> {
        #[derive(Derivative)]
        #[derivative(Debug(bound = ""))]
        enum OverloadToConsider<W: Write + 'static> {
            /// height, cell
            FromCell(ScopeDepth, usize),
            FromFactory(Arc<XType>, XStaticFunction<W>),
        }
        fn prepare_return<W: Write + 'static>(
            namespace: &mut CompilationScope<W>,
            considered: OverloadToConsider<W>,
        ) -> XExpr<W> {
            let new_cell = match considered {
                OverloadToConsider::FromCell(height, cell) => {
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
                    namespace.cells.ipush(Cell::FactoryMadeFunction(t, f))
                }
            };
            XExpr::Value(new_cell)
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
                Overload::Static { spec, cell_idx } => {
                    if dynamic_bind_types.is_some() {
                        continue;
                    }
                    (
                        spec.clone(),
                        OverloadToConsider::FromCell(height, *cell_idx),
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
                    return Ok(prepare_return(self, considered));
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
            return Ok(prepare_return(self, exact_matches.swap_remove(0)));
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
            return Ok(prepare_return(self, generic_matches.swap_remove(0)));
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
type TracedValue = (ScopeDepth, usize);
