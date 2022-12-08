use std::borrow::Cow;
use std::convert::TryInto;
use std::io::Write;
use std::mem;
use std::rc::Rc;
use crate::compilation_scopes::CellSpec;
use crate::{Declaration, let_match, manage_native, RTCell, XOptional, XSequence};
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::{TailedEvalResult, XExpr};
use crate::xvalue::{ManagedXValue, XFunction, XValue};

use derivative::Derivative;
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
    Recourse { depth: StackDepth, scope: XFunction<W> },  // todo
}

impl<W: Write + 'static> EvaluationCell<W> {
    fn from_spec(cell: &CellSpec<W>, parent: Option<&RuntimeScope<W>>, rt: RTCell<W>) -> Result<Self, String> {
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
                        println!("!!! D.0 {:?} {}", ancestor_depth, cell_idx);
                        todo!()
                    }
                    Self::Recourse { depth, scope } => {
                        println!("!!! D.1 {:?} {:?} {}", ancestor_depth, depth, cell_idx);
                        todo!()
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
    defaults: Vec<XExpr<W>>,
    output: Option<Box<XExpr<W>>>,
}

impl<W: Write + 'static> RuntimeScopeTemplate<W> {
    fn to_function(self: Rc<Self>) -> XFunction<W> {
        XFunction::UserFunction {
            defaults: self.defaults.clone(),
            output: self.output.clone().unwrap(),
            template: self,
        }
    }

    pub(crate) fn from_specs(id: usize, name: Option<String>, cell_specs: &[CellSpec<W>], stack_parent: Option<&RuntimeScope<W>>, parent_id: Option<usize>, declarations: Vec<Declaration<W>>, rt: RTCell<W>, defaults: Vec<XExpr<W>>, output: Option<Box<XExpr<W>>>) -> Result<Rc<Self>, String> {
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

        println!("!!! H.0 {name:?} {cell_specs:?}");
        let cells = cell_specs.iter().map(|s| EvaluationCell::from_spec(s, scope_parent, rt.clone())).collect::<Result<_, _>>()?;
        println!("!!! H.1 {name:?} {cell_specs:?} {cells:?}");
        Ok(Rc::new(Self {
            name,
            cells,
            declarations,
            defaults,
            output,
            id,
            scope_parent_height: scope_parent.map(|p|p.height - stack_parent.unwrap().height + 2)
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
    pub(crate) fn from_template(template: Rc<RuntimeScopeTemplate<W>>, stack_parent: Option<&'a Self>, rt: RTCell<W>, mut args: Vec<EvaluatedValue<W>>, defaults: &[XExpr<W>]) -> Result<Rc<Self>, String> {
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

        let default_offset = args.len() - defaults.len();

        for declaration in &template.declarations {
            match declaration {
                Declaration::Parameter { cell_idx, argument_idx } => {
                    let new_value = args.get_mut(*argument_idx).map_or_else(
                        || ret.eval(defaults.get(argument_idx - default_offset).unwrap(), rt.clone(), false).map(|v| v.unwrap_value()),
                        |i| Ok(mem::replace(i, Err("this value has already been used".to_string()))),
                    )?;
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

    pub(crate) fn eval(&self, expr: &XExpr<W>, rt: RTCell<W>, tail_available: bool) -> Result<TailedEvalResult<W>, String> {
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
                let obj = self.eval(obj, rt.clone(), false)?.unwrap_value()?;
                match &obj.as_ref().value {
                    XValue::StructInstance(items) => items[*idx].clone().map(|e| TailedEvalResult::from(e)),
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
                println!("!!! G.0 {cell_idx:?}, {:?}, {raw_value:?}", self.template.cells);
                match raw_value {
                    EvaluationCell::Value(v) => Ok(v.clone()?.into()),
                    EvaluationCell::Uninitialized => panic!("access to uninitialized cell"),
                    EvaluationCell::Recourse { scope, .. } => ManagedXValue::new(XValue::Function(scope.clone()), rt).map(|v| v.into()),
                    EvaluationCell::LocalRecourse => ManagedXValue::new(XValue::Function(self.template.clone().to_function().clone()), rt).map(|v| v.into()),
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
                        if let EvaluationCell::Recourse { depth, scope } = cell {
                            todo!()
                            /*let ud_template = let_match!(scope; XFunction::UserFunction{template, ..} => template);
                            if Rc::ptr_eq(&self.ancestor_at_depth(*depth).template, ud_template){
                                panic!("I'm not sure if this happens or what to do here")
                            }*/
                        }
                    }
                }
                println!("!!! F.0 {:?} {:?}", self.height, callee);
                let callee = self.eval(callee.as_ref(), rt.clone(), false)?.unwrap_value()?;
                println!("!!! F.1 {:?} {:?}", self.height, callee);
                let func = let_match!(&callee.value; XValue::Function(func) => func);
                self.eval_func_with_expressions(func, &args, rt, tail_available)
            }
            XExpr::Lambda(..) => todo!(),
            XExpr::Dummy(v) => v.clone().map(|v| v.into()),
        }
    }

    fn eval_func_with_expressions(&self, func: &XFunction<W>, args: &[XExpr<W>], rt: RTCell<W>, tail_available: bool) -> Result<TailedEvalResult<W>, String> {
        println!("!!! E.1 {:?} {:?} {:?}", self.height, func, args);
        let ret = match func {
            XFunction::Native(nc) => nc(args, self, tail_available, rt),
            XFunction::UserFunction { .. } => {
                let args = args.iter()
                    .map(|e|
                        self.eval(e, rt.clone(), false)
                            .map(|r| r.unwrap_value()))
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_func_with_values(func, args, rt, tail_available)
            }
        };
        println!("!!! E.2 {:?} {:?} {:?} {:?}", self.height, func, args, ret);
        ret
    }

    pub(crate) fn eval_func_with_values(&'a self, func: &XFunction<W>, args: Vec<EvaluatedValue<W>>, rt: RTCell<W>, tail_available: bool) -> Result<TailedEvalResult<W>, String> {
        println!("!!! E.0 {:?} {:?} {:?}", self.height, func, args);
        match func {
            XFunction::Native(..) => {
                let args = args.iter().cloned().map(XExpr::Dummy).collect::<Vec<_>>();
                self.eval_func_with_expressions(func, &args, rt, tail_available)
            }
            XFunction::UserFunction { template, defaults, output } => {
                let mut args = args;
                let mut recursion_depth = 0_usize;
                loop {
                    let scope = Self::from_template(template.clone(), Some(self.clone()), rt.clone(), args, defaults)?;
                    match scope.eval(output.as_ref(), rt.clone(), true)? {
                        TailedEvalResult::TailCall(new_args) => {
                            recursion_depth += 1;
                            if let Some(recursion_limit) = rt.borrow().limits.recursion_limit {
                                if recursion_depth > recursion_limit {
                                    return Err(format!(
                                        "Recursion limit of {} exceeded",
                                        recursion_limit
                                    ));
                                }
                            }
                            args = new_args;
                        }
                        v @ _ => break Ok(v)
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
        println!("!!! C.1 {:?} {depth:?} {cell_idx} {:?}", self.template.name, self.height);
        let ancestor = self.scope_ancestor_at_depth(depth);
        println!("!!! C.2 {:?} {:?}", ancestor.template.name, ancestor.cells);
        (ancestor, &ancestor.cells[cell_idx])
    }

    pub(crate) fn get_cell_value(&self, idx: usize) -> &EvaluationCell<W> {
        self.cells[idx].as_ref(self.template.as_ref())
    }

    fn scope_parent(&self)->Option<&Self>{
        self.template.scope_parent_height.map(|d|self.stack_ancestor_at_depth(d))
    }
}