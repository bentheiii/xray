use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::sync::Arc;
use crate::util::ipush::IPush;
use crate::xexpr::XExpr;
use crate::{CompilationError, Identifier, XFuncSpec, XStaticExpr, XType};
use crate::xvalue::{DynBind, NativeCallable};

/// this is the information stored for a cell during compilation
enum Cell<W: Write + 'static> {
    Recourse,
    /// this can also be a parameter
    Variable(Arc<XType>),
    /// capture will always lead either to a variable, or to a recourse
    Capture { scope_height: usize, cell_idx: usize },
    // scope height,
    FactoryMadeFunction(Rc<XStaticFunction<W>>),
}

impl<W: Write + 'static> From<Cell<W>> for CellSpec<W> {
    fn from(x: Cell<W>) -> Self {
        match x
        {
            Cell::Recourse => Self::Recourse,
            Cell::Variable(..) => Self::Variable,
            Cell::Capture { scope_height, cell_idx } => Self::Capture { scope_height, cell_idx },
            Cell::FactoryMadeFunction(func) => Self::FactoryMadeFunction(func)
        }
    }
}

/// this is the information stored for a cell for it to work during runtime
enum CellSpec<W: Write + 'static> {
    Recourse,
    /// when the function is called, it is the caller's responsibility to fill the
    /// parameter cells with the arguments (including defaults)
    Variable,
    Capture { scope_height: usize, cell_idx: usize },
    // scope height,
    FactoryMadeFunction(Rc<XStaticFunction<W>>),
}

/// these will always point to variable cells
enum Declaration<W: Write + 'static> {
    Parameter { cell_idx: usize, argument_idx: usize },
    Value { cell_idx: usize, expr: XExpr<W> },
    Function { cell_idx: usize, func: XStaticFunction<W> },
}

enum Overload<W: Write + 'static> {
    /// will lead to one of:
    ///  * Cell::Recourse
    ///  * Variable
    Static { cell_idx: usize, spec: XFuncSpec },
    // todo also add declaration line?
    Factory(DynBind<W>),
}

struct CompilationScope<'p, W: Write + 'static> {
    /// in general: the cells of a scope are organized thus:
    /// * params first
    /// * the recursion value, if any
    /// * all the variables, factory made functions
    cells: IPush<Cell<W>>,
    declarations: Vec<Declaration<W>>,

    variables: HashMap<Identifier, usize>,
    // name to cell todo also add declaration line?
    functions: HashMap<Identifier, Vec<Overload<W>>>,
    types: HashMap<Identifier, Arc<XType>>,
    // name to type

    parent: Option<&'p CompilationScope<'p, W>>,
    height: usize,
}

enum XStaticFunction<W: Write + 'static> {
    Native(NativeStaticFunction<W>),
    UserFunction(StaticUserFunction<W>),
}

struct NativeStaticFunction<W: Write + 'static> {
    callback: NativeCallable<W>,
    short_circuit_resolution: bool,
}

struct StaticUserFunction<W: Write + 'static> {
    defaults: Vec<XExpr<W>>,
    cell_specs: Vec<CellSpec<W>>,
    declarations: Vec<Declaration<W>>,
    output: Box<XExpr<W>>, // todo does this have to be a box?
}

impl<'p, W: Write + 'static> CompilationScope<'p, W> {
    fn root() -> Self {
        Self {
            cells: Default::default(),
            declarations: Default::default(),

            variables: Default::default(),
            functions: Default::default(),
            types: Default::default(),

            parent: None,
            height: 0,
        }
    }

    fn from_parent_lambda(parent: &'p CompilationScope<'p, W>, parameters: impl IntoIterator<Item=(Identifier, Arc<XType>)>) -> Self {
        let mut ret = Self {
            parent: Some(parent),
            height: parent.height + 1,

            ..Self::root()
        };
        for (arg_idx, (param_name, param_type)) in parameters.into_iter().enumerate() {
            ret.add_parameter(param_name, arg_idx, param_type);
        }
        ret
    }

    fn from_parent(parent: &'p CompilationScope<'p, W>, parameters: impl IntoIterator<Item=(Identifier, Arc<XType>)>, recourse_name: Identifier, recourse_spec: XFuncSpec) -> Self {
        let mut ret = Self::from_parent_lambda(parent, parameters);
        ret.add_recourse(recourse_name, recourse_spec);
        ret
    }

    fn add_recourse(&mut self, name: Identifier, spec: XFuncSpec) {
        // todo assert that no values/types use the identifier
        let cell_idx = self.cells.ipush(Cell::Recourse); // this is really always supposed to be 0, assert?
        self.functions.entry(name).or_default().push(Overload::Static { cell_idx, spec });
    }

    fn add_static_func(&mut self, name: Identifier, spec: XFuncSpec, func: XStaticFunction<W>) {
        // todo assert that no values/types use the identifier
        let cell_idx = self.cells.ipush(Cell::Variable(spec.xtype2()));
        self.functions.entry(name).or_default().push(Overload::Static { cell_idx, spec });
        self.declarations.push(Declaration::Function { cell_idx, func })
    }

    fn add_variable(&mut self, name: Identifier, expr: XExpr<W>, xtype: Arc<XType>) {
        // todo assert that no overloads/types use the identifier
        let cell_idx = self.cells.ipush(Cell::Variable(xtype));
        self.variables.insert(name, cell_idx);
        self.declarations.push(Declaration::Value { cell_idx, expr })
    }

    fn add_parameter(&mut self, name: Identifier, arg_idx: usize, xtype: Arc<XType>) {
        // todo assert that no overloads/types use the identifier
        let cell_idx = self.cells.ipush(Cell::Variable(xtype));
        self.variables.insert(name, cell_idx);
        self.declarations.push(Declaration::Parameter { cell_idx, argument_idx: arg_idx })
    }

    fn to_static_ud(self, defaults: Vec<XExpr<W>>, output: Box<XExpr<W>>) -> StaticUserFunction<W> {
        StaticUserFunction {
            defaults,
            cell_specs: self.cells.into_iter().map(|&c| c.into()).collect(),
            declarations: self.declarations,
            output,
        }
    }

    fn compile(&mut self, stat_expr: XStaticExpr) -> Result<XExpr<W>, CompilationError<W>> {
        match stat_expr {
            XStaticExpr::LiteralBool(v) => Ok(XExpr::LiteralBool(v)),
            XStaticExpr::LiteralInt(v) => Ok(XExpr::LiteralInt(v)),
            XStaticExpr::LiteralFloat(v) => Ok(XExpr::LiteralFloat(v)),
            XStaticExpr::LiteralString(v) => Ok(XExpr::LiteralString(v)),
            XStaticExpr::Array(items) => Ok(XExpr::Array(
                items.into_iter().map(|i| self.compile(i)).collect::<Result<_, _>>()?
            )),
            XStaticExpr::Member(obj, member_name) => {
                let obj = self.compile(*obj)?;
                let obj_type = obj.xtype()?;
                let member_idx = match obj_type.as_ref() {
                    XType::Compound(_, spec, _) => {
                        if let Some(&index) = spec.indices.get(&member_name) {
                            index
                        } else {
                            return Err(CompilationError::MemberNotFound {
                                spec: spec.clone(),
                                name: member_name.clone(),
                            });
                        }
                    }
                    XType::Tuple(types) => {
                        let idx = member_name
                            .strip_prefix("item")
                            .map(|s| s.parse::<usize>().unwrap())
                            .ok_or_else(|| CompilationError::NonItemTupleAccess {
                                member: member_name.clone(),
                            })?;
                        if idx > types.len() {
                            return Err(CompilationError::TupleIndexOutOfBounds {
                                tuple_type: obj_type.clone(),
                                index: idx,
                                max: types.len(),
                            });
                        } else {
                            idx
                        }
                    }
                    _ => return Err(CompilationError::NonCompoundMemberAccess {
                        xtype: obj_type,
                    }),
                };
                Ok(XExpr::Member(Box::new(obj), member_idx))
            }
            XStaticExpr::Tuple(items) => Ok(XExpr::Tuple(
                items.into_iter().map(|i| self.compile(i)).collect::<Result<_, _>>()?
            )),
            XStaticExpr::Lambda(args, output) => {
                let mut subscope = CompilationScope::from_parent_lambda(self, args.iter().map(|arg| (arg.name, arg.type_.clone())));
                let output = subscope.compile(*output)?;
                let defaults = args.into_iter().filter_map(|a| a.default.map(|s| subscope.compile(s))).collect::<Result<_,_>>()?;
                let ud_func = subscope.to_static_ud(defaults, Box::new(output));
                todo!() // Ok(XExpr::Lambda(ud_func))
            }
            XStaticExpr::SpecializedIdent(..) => todo!(),
            XStaticExpr::Ident(..) => todo!(),
            XStaticExpr::Call(..) => todo!(),

            _ => { unreachable!() }
        }
    }
}