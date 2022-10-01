use std::borrow::Cow;
use std::io::Write;
use std::mem;
use std::rc::Rc;
use crate::compilation_scopes::CellSpec;
use crate::{Declaration, let_match, manage_native, RTCell, XOptional, XSequence};
use crate::util::lazy_bigint::LazyBigint;
use crate::xexpr::{TailedEvalResult, XExpr};
use crate::xvalue::{ManagedXValue, XFunction, XValue};

use derivative::Derivative;

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

type EvaluatedValue<W> = Result<Rc<ManagedXValue<W>>, String>;

#[derive(Default, Derivative)]
#[derivative(Debug(bound = ""))]
pub(crate) enum EvaluationCell<W: Write + 'static> {
    #[default]
    Uninitialized,
    Value(EvaluatedValue<W>),
    LocalRecourse,
    Recourse { height: usize, scope: XFunction<W> },  // todo
}

impl<W: Write + 'static> EvaluationCell<W> {
    fn from_spec(cell: &CellSpec<W>, parent: Option<&RuntimeScope<W>>, rt: RTCell<W>) -> Result<Self, String> {
        match cell {
            CellSpec::Variable => Ok(Self::Uninitialized),
            CellSpec::Recourse => Ok(Self::LocalRecourse),
            CellSpec::FactoryMadeFunction(native) => Ok(Self::Value(Ok(ManagedXValue::new(XValue::Function(XFunction::Native(native.clone())), rt)?))),
            CellSpec::Capture { scope_height, cell_idx } => {
                let (ancestor, cell) = parent.unwrap().ancestor_and_cell(*scope_height, *cell_idx);
                match cell.as_ref(ancestor.template.as_ref()) {
                    Self::Uninitialized => panic!("cannot capture uninitialized data"),
                    Self::Value(v) => Ok(Self::Value(v.clone())),
                    Self::LocalRecourse => Ok(Self::Recourse {
                        height: *scope_height,
                        scope: ancestor.template.clone().to_function(),
                    }),
                    Self::Recourse { height, scope } => Ok(Self::Recourse { height: *height, scope: scope.clone() })
                }
            }
        }
    }
}

pub struct RuntimeScopeTemplate<W: Write + 'static> {
    pub(crate) cells: Vec<EvaluationCell<W>>,
    pub(crate) declarations: Vec<Declaration<W>>,
    defaults: Vec<XExpr<W>>,
    output: Option<Box<XExpr<W>>>,
}

impl<W: Write + 'static> RuntimeScopeTemplate<W> {
    fn to_function(self: Rc<Self>)->XFunction<W>{
        XFunction::UserFunction {
            defaults: self.defaults.clone(),
            output: self.output.clone().unwrap(),
            template: self,
        }
    }

    pub(crate) fn from_specs(cell_specs: &[CellSpec<W>], parent: Option<&RuntimeScope<W>>, declarations: Vec<Declaration<W>>, rt: RTCell<W>, defaults: Vec<XExpr<W>>, output: Option<Box<XExpr<W>>>) -> Result<Rc<Self>, String> {
        let cells = cell_specs.iter().map(|s| EvaluationCell::from_spec(s, parent, rt.clone())).collect::<Result<_, _>>()?;
        Ok(Rc::new(Self {
            cells,
            declarations,
            defaults,
            output
        }))
    }
}


pub struct RuntimeScope<'a, W: Write + 'static> {
    pub(crate) cells: Vec<TemplatedEvaluationCell<W>>,
    height: usize,
    parent: Option<&'a Self>,
    template: Rc<RuntimeScopeTemplate<W>>,
}

impl<'a, W: Write + 'static> RuntimeScope<'a, W> {
    pub(crate) fn from_template(template: Rc<RuntimeScopeTemplate<W>>, parent: Option<&'a Self>, rt: RTCell<W>, mut args: Vec<EvaluatedValue<W>>, defaults: &[XExpr<W>]) -> Result<Rc<Self>, String> {
        let mut ret = Self {
            cells: template.cells.iter().enumerate().map(|(idx, c)|
                if let EvaluationCell::Uninitialized = c {
                    TemplatedEvaluationCell::Owned(EvaluationCell::Uninitialized)
                } else {
                    TemplatedEvaluationCell::FromTemplate(idx)
                }
            ).collect(),
            height: parent.map_or(0, |p| p.height + 1),
            parent,
            template: template.clone(),
        };

        let default_offset = args.len() - defaults.len();

        for declaration in &template.declarations {
            match declaration {
                Declaration::Parameter { cell_idx, argument_idx } => {
                    let new_value = args.get_mut(*argument_idx).map_or_else(
                        || ret.eval(defaults.get(argument_idx - default_offset).unwrap(), rt.clone(), false).map(|v| v.unwrap_value()),
                        |i| Ok(mem::replace(i, Err("this value has already been used".to_string())))
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
            },
            XExpr::Call(callee, args) => {
                // special case: tca through recursion
                if tail_available{
                    if let XExpr::Value(cell_idx) = callee.as_ref(){
                        let cell = self.get_cell_value(*cell_idx);
                        if let EvaluationCell::LocalRecourse = cell{
                            let args = args
                                        .iter()
                                        .map(|x| {
                                            self.eval(x, rt.clone(), false)
                                                .map(|r| r.unwrap_value())
                                        })
                                        .collect::<Result<_, _>>()?;
                            return Ok(TailedEvalResult::TailCall(args))
                        }
                        if let EvaluationCell::Recourse {height, scope} = cell{
                            let ud_template = let_match!(scope; XFunction::UserFunction{template, ..} => template);
                            if Rc::ptr_eq(&self.ancestor_at_height(*height).template, ud_template){
                                panic!("I'm not sure if this happens or what to do here")
                            }
                        }
                    }
                }
                let callee = self.eval(callee.as_ref(), rt.clone(), false)?.unwrap_value()?;
                let func = let_match!(&callee.value; XValue::Function(func) => func);
                self.eval_func_with_expressions(func, &args, rt, tail_available)
            }
            XExpr::Lambda(..) => todo!(),
            XExpr::Dummy(v) => v.clone().map(|v| v.into()),
        }
    }

    fn eval_func_with_expressions(&self, func: &XFunction<W>, args: &[XExpr<W>], rt: RTCell<W>, tail_available: bool)->Result<TailedEvalResult<W>, String>{
        match func {
            XFunction::Native(nc) => nc(args, self, tail_available, rt),
            XFunction::UserFunction {..} => {
                let args = args.iter()
                    .map(|e|
                        self.eval(e, rt.clone(), false)
                            .map(|r| r.unwrap_value()))
                    .collect::<Result<Vec<_>, _>>()?;
                self.eval_func_with_values(func, args, rt, tail_available)
            },
        }
    }

    pub(crate) fn eval_func_with_values(&'a self, func: &XFunction<W>, args: Vec<EvaluatedValue<W>>, rt: RTCell<W>, tail_available: bool)->Result<TailedEvalResult<W>, String>{
        match func {
            XFunction::Native(..) => {
                let args = args.iter().cloned().map(XExpr::Dummy).collect::<Vec<_>>();
                self.eval_func_with_expressions(func, &args, rt, tail_available)
            },
            XFunction::UserFunction {template, defaults, output} => {
                let mut args = args;
                let mut recursion_depth = 0_usize;
                loop {
                    let scope = Self::from_template(template.clone(), Some(self.clone()), rt.clone(), args, defaults)?;
                    match scope.eval(output.as_ref(), rt.clone(), true)?{
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
            },
        }
    }

    fn ancestor_at_height(&self, height: usize) -> &Self {
        let mut current = self;
        while current.height != height {
            current = current.parent.expect("ran out of parents at runtime");
        }
        current
    }


    fn ancestor_and_cell(&self, height: usize, cell_idx: usize) -> (&Self, &TemplatedEvaluationCell<W>) {
        let ancestor = self.ancestor_at_height(height);
        (ancestor, &ancestor.cells[cell_idx])
    }

    pub(crate) fn get_cell_value(&self, idx: usize) -> &EvaluationCell<W>{
        self.cells[idx].as_ref(self.template.as_ref())
    }
}