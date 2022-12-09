

use std::io::Write;
use std::mem;
use std::rc::Rc;
use crate::compilation_scope::CellSpec;
use crate::{Declaration, let_match, manage_native, RTCell, XOptional, xraise, XSequence};
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::{TailedEvalResult, XExpr};
use crate::xvalue::{ManagedXValue, XFunction, XValue};

use derivative::Derivative;
use crate::runtime_err::RuntimeError;
use crate::units::{ScopeDepth, StackDepth};

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum TemplatedEvaluationCell<W: Write + 'static> {
    Owned(EvaluationCell<W>),
    FromTemplate(usize),
}

impl<W: Write + 'static> TemplatedEvaluationCell<W> {
    fn put(&mut self, value: EvaluatedValue<W>) {
        match self {
            Self::FromTemplate(..) => panic!("attempted to write to templated cell"),
            Self::Owned(ref mut v) => {
                *v = EvaluationCell::Value(value);
            }
        }
    }
    fn as_ref<'a>(&'a self, template: &'a RuntimeScopeTemplate<W>) -> &'a EvaluationCell<W> {
        match self {
            Self::FromTemplate(idx) => &template.cells[*idx],
            Self::Owned(r) => r
        }
    }
}

pub type EvaluatedValue<W> = Result<Rc<ManagedXValue<W>>, String>;

#[derive(Default, Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum EvaluationCell<W: Write + 'static> {
    #[default]
    Uninitialized,
    Value(EvaluatedValue<W>),
    LocalRecourse,
    Recourse { depth: ScopeDepth, scope: XFunction<W> },  // todo
}

impl<W: Write + 'static> EvaluationCell<W> {
    fn from_spec(cell: &CellSpec<W>, parent: Option<&RuntimeScope<W>>, rt: RTCell<W>) -> Result<Self, RuntimeError> {
        match cell {
            CellSpec::Variable => Ok(Self::Uninitialized),
            CellSpec::Recourse => Ok(Self::LocalRecourse),
            CellSpec::FactoryMadeFunction(native) => Ok(Self::Value(Ok(ManagedXValue::new(XValue::Function(XFunction::Native(native.clone())), rt)?))),
            CellSpec::Capture { ancestor_depth, cell_idx } => {
                // note that the parent is already one-deep so we need to subtract one from the depth
                let (ancestor, cell) = parent.unwrap().scope_ancestor_and_cell(*ancestor_depth-1, *cell_idx);
                match cell.as_ref(ancestor.template.as_ref()) {
                    Self::Uninitialized => {
                        panic!("cannot capture uninitialized data")
                    }
                    Self::Value(v) => Ok(Self::Value(v.clone())),
                    Self::LocalRecourse => {
                        Ok(Self::Recourse {depth: *ancestor_depth, scope: ancestor.template.clone().to_function()})
                    }
                    Self::Recourse { .. } => {
                        panic!("I don't think this happens")
                    }
                }
            }
        }
    }
}

pub struct RuntimeScopeTemplate<W: Write + 'static> {
    pub(crate) id: usize,
    pub(crate) name: Option<String>,
    pub(crate) cells: Vec<EvaluationCell<W>>,
    pub(crate) declarations: Vec<Declaration<W>>,
    pub(crate) scope_parent_height: Option<StackDepth>, // todo just use parent id?
    pub(crate) param_count: usize,
    defaults: Vec<EvaluatedValue<W>>,
    output: Option<Box<XExpr<W>>>,
}

impl<W: Write + 'static> RuntimeScopeTemplate<W> {
    fn to_function(self: Rc<Self>) -> XFunction<W> {
        XFunction::UserFunction {
            output: self.output.clone().unwrap(),
            template: self,
        }
    }

    #[allow(clippy::too_many_arguments)] // todo
    pub(crate) fn from_specs(id: usize, name: Option<String>, param_count: usize, cell_specs: &[CellSpec<W>], stack_parent: Option<&RuntimeScope<W>>, parent_id: Option<usize>, declarations: Vec<Declaration<W>>, rt: RTCell<W>, defaults: Vec<XExpr<W>>, output: Option<Box<XExpr<W>>>) -> Result<Rc<Self>, RuntimeError> {
        // in order to get the namespace parent, we need to go up the stack until we reach one with the same id
        let scope_parent = if let Some(parent_id) = parent_id {
            {
                let mut ancestor = stack_parent;
                loop {
                    match ancestor {
                        None => break ancestor,
                        Some(p) if p.template.id == parent_id => break ancestor,
                        Some(p) => ancestor = p.stack_parent
                    }
                }
            }
        } else {None};

        let cells = cell_specs.iter().map(|s| EvaluationCell::from_spec(s, scope_parent, rt.clone())).collect::<Result<_, _>>()?;
        let defaults = defaults.iter().map(|d| scope_parent.unwrap().eval(d, rt.clone(), false).map(|r| r.unwrap_value())).collect::<Result<_, _>>()?;
        Ok(Rc::new(Self {
            name,
            cells,
            declarations,
            defaults,
            output,
            id,
            param_count,
            scope_parent_height: scope_parent.map(|p|p.height)
        }))
    }
}


pub struct RuntimeScope<'a, W: Write + 'static> {
    pub(crate) cells: Vec<TemplatedEvaluationCell<W>>,
    height: StackDepth,
    stack_parent: Option<&'a Self>,
    template: Rc<RuntimeScopeTemplate<W>>,
}

impl<'a, W: Write + 'static> RuntimeScope<'a, W> {
    pub(crate) fn from_template(template: Rc<RuntimeScopeTemplate<W>>, stack_parent: Option<&'a Self>, rt: RTCell<W>, mut args: Vec<EvaluatedValue<W>>) -> Result<Rc<Self>, RuntimeError> {
        let mut ret = Self {
            cells: template.cells.iter().enumerate().map(|(idx, c)|
                if let EvaluationCell::Uninitialized = c {
                    TemplatedEvaluationCell::Owned(EvaluationCell::Uninitialized)
                } else {
                    TemplatedEvaluationCell::FromTemplate(idx)
                }
            ).collect(),
            height: stack_parent.map_or(StackDepth(0), |p| p.height + 1),
            stack_parent,
            template: template.clone(),
        };

        let default_offset = template.param_count - template.defaults.len();

        for declaration in &template.declarations {
            match declaration {
                Declaration::Parameter { cell_idx, argument_idx } => {
                    let new_value = args.get_mut(*argument_idx).map_or_else(
                        || template.defaults[argument_idx - default_offset].clone(),
                        |i| mem::replace(i, Err("this value has already been used".to_string())),
                    );
                    ret.cells[*cell_idx].put(new_value);
                }
                Declaration::Value { cell_idx, expr } => {
                    let new_value = ret.eval(expr, rt.clone(), false)?.unwrap_value();
                    ret.cells[*cell_idx].put(new_value);
                }
                Declaration::Function { cell_idx, func } => {
                    let new_value = Ok(ManagedXValue::new(XValue::Function(func.to_function(&ret, rt.clone())?), rt.clone())?);
                    ret.cells[*cell_idx].put(new_value)
                }
            }
        }

        Ok(Rc::new(ret))
    }

    pub(crate) fn eval(&self, expr: &XExpr<W>, rt: RTCell<W>, tail_available: bool) -> Result<TailedEvalResult<W>, RuntimeError> {
        match expr {
            XExpr::LiteralBool(b) => Ok(ManagedXValue::new(XValue::Bool(*b), rt)?.into()),
            XExpr::LiteralInt(i) => {
                Ok(ManagedXValue::new(XValue::Int(LazyBigint::from(*i)), rt)?.into())
            }
            XExpr::LiteralFloat(r) => Ok(ManagedXValue::new(XValue::Float(*r), rt)?.into()),
            XExpr::LiteralString(s) => {
                Ok(ManagedXValue::new(XValue::String(s.clone()), rt)?.into())
            }
            XExpr::Array(items) => {
                let seq = if items.is_empty() {
                    XSequence::Empty
                } else {
                    XSequence::array(
                        items
                            .iter()
                            .map(|x| {
                                self.eval(x, rt.clone(), false)
                                    .map(|r| r.unwrap_value())
                            })
                            .collect::<Result<_, _>>()?,
                    )
                };
                Ok(ManagedXValue::new(XValue::Native(Box::new(seq)), rt)?.into())
            }
            XExpr::Construct(_, _, items) | XExpr::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|x| {
                        self.eval(x, rt.clone(), false)
                            .map(|r| r.unwrap_value())
                    })
                    .collect::<Result<_, _>>()?;
                Ok(ManagedXValue::new(XValue::StructInstance(items), rt)?.into())
            }
            XExpr::Member(obj, idx) => {
                let obj = xraise!(self.eval(obj, rt.clone(), false)?.unwrap_value());
                match &obj.as_ref().value {
                    XValue::StructInstance(items) => Ok(TailedEvalResult::from(items[*idx].clone())),
                    XValue::UnionInstance(variant, item) => Ok(if variant == idx {
                        manage_native!(
                            XOptional {
                                value: Some(item.clone())
                            },
                            rt
                        )
                    } else {
                        manage_native!(XOptional::<W> { value: None }, rt)
                    }),
                    _ => panic!("Expected struct, got {:?}", obj),
                }
            }
            XExpr::Value(cell_idx) => {
                let raw_value = self.get_cell_value(*cell_idx);
                match raw_value {
                    EvaluationCell::Value(v) => Ok(v.clone().into()),
                    EvaluationCell::Uninitialized => panic!("access to uninitialized cell"),
                    EvaluationCell::Recourse { scope, .. } => ManagedXValue::new(XValue::Function(scope.clone()), rt).map(|v| v.into()),
                    EvaluationCell::LocalRecourse => ManagedXValue::new(XValue::Function(self.template.clone().to_function()), rt).map(|v| v.into()),
                }
            }
            XExpr::Variant(.., idx, expr) => {
                let obj = self.eval(expr, rt.clone(), false)?.unwrap_value();
                Ok(ManagedXValue::new(XValue::UnionInstance(*idx, obj), rt)?.into())
            }
            XExpr::Call(callee, args) => {
                // special case: tca through recursion
                if tail_available {
                    if let XExpr::Value(cell_idx) = callee.as_ref() {
                        let cell = self.get_cell_value(*cell_idx);
                        if let EvaluationCell::LocalRecourse = cell {
                            let args = args
                                .iter()
                                .map(|x| {
                                    self.eval(x, rt.clone(), false)
                                        .map(|r| r.unwrap_value())
                                })
                                .collect::<Result<_, _>>()?;
                            return Ok(TailedEvalResult::TailCall(args));
                        }
                        // todo can we recurse a parent call?
                    }
                }
                let callee = xraise!(self.eval(callee.as_ref(), rt.clone(), false)?.unwrap_value());
                let func = let_match!(&callee.value; XValue::Function(func) => func);
                self.eval_func_with_expressions(func, args, rt, tail_available)
            }
            XExpr::Dummy(v) => Ok(TailedEvalResult::from(v.clone())),
        }
    }

    fn eval_func_with_expressions(&self, func: &XFunction<W>, args: &[XExpr<W>], rt: RTCell<W>, tail_available: bool) -> Result<TailedEvalResult<W>, RuntimeError> {
        match func {
            XFunction::Native(nc) => nc(args, self, tail_available, rt),
            XFunction::UserFunction { .. } => {
                let args = args.iter()
                    .map(|e|
                        self.eval(e, rt.clone(), false)
                            .map(|r| r.unwrap_value()))
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_func_with_values(func, args, rt, tail_available)
            }
        }
    }

    pub(crate) fn eval_func_with_values(&'a self, func: &XFunction<W>, args: Vec<EvaluatedValue<W>>, rt: RTCell<W>, tail_available: bool) -> Result<TailedEvalResult<W>, RuntimeError> {
        match func {
            XFunction::Native(..) => {
                let args = args.iter().cloned().map(XExpr::Dummy).collect::<Vec<_>>();
                self.eval_func_with_expressions(func, &args, rt, tail_available)
            }
            XFunction::UserFunction { template, output } => {
                let mut args = args;
                let mut recursion_depth = 0_usize;
                loop {
                    let scope = Self::from_template(template.clone(), Some(self), rt.clone(), args)?;
                    let v = scope.eval(output.as_ref(), rt.clone(), true);
                    match v? {
                        TailedEvalResult::TailCall(new_args) => {
                            recursion_depth += 1;
                            if let Some(recursion_limit) = rt.borrow().limits.recursion_limit {
                                if recursion_depth > recursion_limit {
                                    return Err(RuntimeError::MaximumRecursion);
                                }
                            }
                            args = new_args;
                        }
                        v => {
                            break Ok(v)
                        }
                    }
                }
            }
        }
    }

    fn stack_ancestor_at_depth(&self, depth: StackDepth) -> &Self {
        let mut current = self;
        for _ in 0..depth.0 {
            current = current.stack_parent.expect("ran out of stack parents at runtime");
        }
        current
    }

    fn scope_ancestor_at_depth(&self, depth: ScopeDepth) -> &Self {
        let mut current = self;
        for _ in 0..depth.0 {
            current = current.scope_parent().expect("ran out of scope parents at runtime");
        }
        current
    }

    fn scope_ancestor_and_cell(&self, depth: ScopeDepth, cell_idx: usize) -> (&Self, &TemplatedEvaluationCell<W>) {
        let ancestor = self.scope_ancestor_at_depth(depth);
        (ancestor, &ancestor.cells[cell_idx])
    }

    pub(crate) fn get_cell_value(&self, idx: usize) -> &EvaluationCell<W> {
        self.cells[idx].as_ref(self.template.as_ref())
    }

    // todo just make this a field?
    fn scope_parent(&self)->Option<&Self>{
        self.template.scope_parent_height.map(|d|self.stack_ancestor_at_depth(self.height-d))
    }
}