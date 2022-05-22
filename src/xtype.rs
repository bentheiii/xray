use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use derivative::Derivative;
use string_interner::{DefaultSymbol, StringInterner};
use crate::CompilationError;
use crate::native_types::NativeType;
use crate::xscope::Identifier;

#[derive(Debug, Eq)]
pub enum XType {
    Bool,
    Int,
    Rational,
    String,
    XUnknown,
    Compound(CompoundKind, Arc<XCompoundSpec>, Bind),
    XCallable(XCallableSpec),
    XFunc(XFuncSpec),
    XGeneric(Identifier),
    XNative(Box::<dyn NativeType>, Vec<Arc<XType>>),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum CompoundKind {
    Struct,
    Union,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Bind {
    bound_generics: HashMap<Identifier, Arc<XType>>,
}

impl Bind {
    pub fn new() -> Self {
        Bind {
            bound_generics: HashMap::new(),
        }
    }

    pub fn from<T: Into<HashMap<Identifier, Arc<XType>>>>(bound_generics: T) -> Self {
        Bind {
            bound_generics: bound_generics.into(),
        }
    }


    pub fn mix(mut self, other: &Bind) -> Option<Self> {
        for (k, v) in other.bound_generics.iter() {
            if let Some(existing) = self.bound_generics.get(k) {
                let new_bind = existing.bind_in_assignment(v)?;
                self = self.mix(&new_bind)?;
            } else {
                self.bound_generics.insert(k.clone(), v.clone());
            }
        }
        Some(self)
    }

    pub fn get(&self, id: &Identifier) -> Option<&Arc<XType>> {
        self.bound_generics.get(id)
    }

    pub fn iter(&self) -> impl Iterator<Item=(&Identifier, &Arc<XType>)> {
        self.bound_generics.iter()
    }
}


#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XCompoundSpec {
    pub name: Identifier,
    // this is the full qualified name
    pub fields: Vec<XCompoundFieldSpec>,
    pub indices: BTreeMap<String, usize>,
}

impl XCompoundSpec {
    pub fn new(name: Identifier, fields: Vec<XCompoundFieldSpec>) -> XCompoundSpec {
        let mut indices = BTreeMap::new();
        for (i, field) in fields.iter().enumerate() {
            indices.insert(field.name.clone(), i);
        }
        XCompoundSpec {
            name,
            fields,
            indices,
        }
    }

    pub fn bind(&self, args: &Vec<Arc<XType>>) -> Option<Bind> {
        if args.len() != self.fields.len() {
            return None;
        }
        let mut ret = Bind::new();
        for (arg, param) in args.into_iter().zip(self.fields.iter()) {
            ret = ret.mix(&param.type_.bind_in_assignment(&arg)?)?;
        }
        Some(ret)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XCompoundFieldSpec {
    pub name: String,
    #[derivative(Hash = "ignore")]
    pub type_: Arc<XType>,
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XCallableSpec {
    #[derivative(Hash = "ignore")]
    pub param_types: Vec<Arc<XType>>,
    #[derivative(Hash = "ignore")]
    pub return_type: Arc<XType>,
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XFuncSpec {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub params: Vec<XFuncParamSpec>,
    #[derivative(Hash = "ignore")]
    pub ret: Arc<XType>,
}

impl XFuncSpec {
    pub fn arg_len_range(&self) -> (usize, usize) {
        let max = self.params.len();
        let min = self.params.iter().take_while(|a| a.required).count();
        (min, max)
    }

    pub fn bind(&self, args: &Vec<Arc<XType>>) -> Option<Bind> {
        let (min, max) = self.arg_len_range();
        if args.len() < min || args.len() > max {
            return None;
        }
        let mut ret = Bind::new();
        for (arg, param) in args.into_iter().zip(self.params.iter()) {
            ret = ret.mix(&param.type_.bind_in_assignment(&arg)?)?;
        }
        Some(ret)
    }

    pub fn rtype(&self, bind: &Bind) -> Arc<XType> {
        self.ret.clone().resolve_bind(bind)
    }

    pub fn xtype(&self, bind: &Bind) -> Arc<XType> {
        Arc::new(XType::XFunc(
            XFuncSpec {
                generic_params: None,
                params: self.params.iter().map(|p| {
                    XFuncParamSpec {
                        type_: p.type_.clone().resolve_bind(bind),
                        required: p.required,
                    }
                }).collect(),
                ret: self.ret.clone().resolve_bind(bind),
            }
        ))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XFuncParamSpec {
    #[derivative(Hash = "ignore")]
    pub type_: Arc<XType>,
    pub required: bool,
}

impl XType {
    pub fn generic_from_name(name: &'static str, interner: &mut StringInterner) -> Arc<XType> {
        Arc::new(XType::XGeneric(interner.get_or_intern_static(name)))
    }

    pub fn bind_in_assignment(&self, other: &Arc<XType>) -> Option<Bind> {
        match (self, other.as_ref()) {
            (XType::Bool, XType::Bool) => Some(Bind::new()),
            (XType::Int, XType::Int) => Some(Bind::new()),
            (XType::Rational, XType::Rational) => Some(Bind::new()),
            (XType::String, XType::String) => Some(Bind::new()),
            (XType::Compound(k0, a, ref bind_a), XType::Compound(k1, b, ref bind_b))=> {
                if a != b || k0 != k1{
                    return None;
                }
                let mut bind = Bind::new();
                for p_type in a.fields.iter().map(|f| f.type_.clone()) {
                    if let Some(binds) = p_type.clone().resolve_bind(bind_a).bind_in_assignment(&p_type.resolve_bind(bind_b)) {
                        bind = bind.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                Some(bind)
            }
            (XType::XCallable(ref a), XType::XCallable(ref b)) => {
                let mut total_binds = Bind::new();
                for (a_type, b_type) in a.param_types.iter().zip(b.param_types.iter()) {
                    if let Some(binds) = a_type.bind_in_assignment(&b_type) {
                        total_binds = total_binds.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.return_type.bind_in_assignment(&b.return_type) {
                    total_binds = total_binds.mix(&binds)?;
                } else {
                    return None;
                }
                Some(total_binds)
            }
            (XType::XFunc(ref a), XType::XFunc(ref b)) => {
                let (a_min, a_max) = a.arg_len_range();
                let (b_min, b_max) = b.arg_len_range();
                if a_min < b_min || a_max > b_max {
                    return None;
                }
                let mut total_binds = Bind::new();
                for (a_arg, b_arg) in a.params.iter().zip(b.params.iter()) {
                    if let Some(binds) = a_arg.type_.bind_in_assignment(&b_arg.type_) {
                        total_binds = total_binds.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.ret.bind_in_assignment(&b.ret) {
                    total_binds = total_binds.mix(&binds)?;
                } else {
                    return None;
                }
                Some(total_binds)
            }
            (XType::XCallable(ref a), XType::XFunc(ref b)) => {
                let (b_min, b_max) = b.arg_len_range();
                if a.param_types.len() < b_min || a.param_types.len() > b_max {
                    return None;
                }
                let mut total_binds = Bind::new();
                for (a_type, b_arg) in a.param_types.iter().zip(b.params.iter()) {
                    if let Some(binds) = a_type.bind_in_assignment(&b_arg.type_) {
                        total_binds = total_binds.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.return_type.bind_in_assignment(&b.ret) {
                    total_binds = total_binds.mix(&binds)?;
                } else {
                    return None;
                }
                Some(total_binds)
            }
            (XType::XNative(a, a_bind), XType::XNative(b, b_bind)) => {
                if a != b {
                    return None;
                }
                let mut bind = Bind::new();
                for (a_v, b_v) in a_bind.iter().zip(b_bind.iter()) {
                    // since the types are equal we can assume they have the same keys at binding
                    if let Some(binds) = a_v.bind_in_assignment(b_v) {
                        bind = bind.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                Some(bind)
            }
            (_, XType::XUnknown) => Some(Bind::new()),
            (XType::XUnknown, _) => Some(Bind::new()),
            (XType::XGeneric(ref a), XType::XGeneric(ref b)) if a == b => {
                Some(Bind::new())
            }
            (XType::XGeneric(ref a), _) => {
                Some(Bind::from([
                    (a.clone(), other.clone()),
                ]))
            }

            _ => {
                None
            }
        }
    }
    pub fn resolve_bind(self: Arc<XType>, bind: &Bind) -> Arc<XType> {
        match self.as_ref() {
            XType::XNative(a, ref a_bind) => {
                let mut new_bind = HashMap::new();
                for (k, a_v) in a.generic_names().iter().zip(a_bind.iter()) {
                    new_bind.insert(k.clone(), a_v.clone().resolve_bind(bind));
                }
                XType::XNative(a.clone(), a.generic_names().iter().map(|n| new_bind.get(n).unwrap()).cloned().collect()).into()
            }
            XType::XGeneric(ref a) => bind.get(a).map(|b| b.clone()).unwrap_or(self.clone()),
            _ => self,
        }
    }

    pub fn is_unknown(self: &Arc<XType>) -> bool {
        match self.as_ref() {
            XType::XUnknown => true,
            XType::XNative(_, types) => types.iter().cloned().any(|t| t.is_unknown()),
            XType::XCallable(spec) => spec.param_types.iter().cloned().any(|t| t.is_unknown()) || spec.return_type.is_unknown(),
            XType::XFunc(spec) => spec.params.iter().any(|t| t.type_.clone().is_unknown()) || spec.ret.is_unknown(),
            XType::Compound(.., bind) => bind.iter().any(|(_, t)| t.is_unknown()),
            _ => false,
        }
    }
}

impl PartialEq<XType> for XType {
    fn eq(&self, other: &XType) -> bool {
        match (self, other) {
            (XType::Bool, XType::Bool) => true,
            (XType::Int, XType::Int) => true,
            (XType::Rational, XType::Rational) => true,
            (XType::String, XType::String) => true,
            (XType::Compound(k0, ref a, ref a_b), XType::Compound(k1, ref b, ref b_b)) => k0==k1 && a.name == b.name && a_b == b_b,
            (XType::XCallable(ref a), XType::XCallable(ref b)) => a.eq(b),
            (XType::XFunc(ref a), XType::XFunc(ref b)) => a.generic_params == b.generic_params && a.params.len() == b.params.len() && a.params.iter().zip(b.params.iter()).all(|(a, b)| a.type_.eq(&b.type_)),
            (XType::XCallable(ref a), XType::XFunc(ref b)) => b.generic_params.is_none() && a.param_types == b.params.iter().map(|p| p.type_.clone()).collect::<Vec<_>>() && a.return_type.eq(&b.ret),
            (XType::XFunc(_), XType::XCallable(_)) => other == self,
            (XType::XUnknown, XType::XUnknown) => true,
            (XType::XGeneric(ref a), XType::XGeneric(ref b)) => a == b,
            (XType::XNative(a, ref a_bind), XType::XNative(b, ref b_bind)) => a == b && a_bind == b_bind,
            _ => false,
        }
    }
}

impl Display for XType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            XType::Bool => write!(f, "bool"),
            XType::Int => write!(f, "int"),
            XType::Rational => write!(f, "rational"),
            XType::String => write!(f, "string"),
            XType::Compound(_, ref a, ref b) => write!(f, "{:?}<{:?}>", a.name, b),
            XType::XCallable(ref a) => write!(f, "{:?}->{}", a.param_types, a.return_type),
            XType::XFunc(ref a) => {
                write!(f, "(")?;
                for (i, arg) in a.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}{}", arg.type_, if arg.required { "" } else { "?" })?;
                }
                write!(f, ") -> {}", a.ret)
            }
            XType::XGeneric(ref a) => write!(f, "{:?}", a), // TODO
            XType::XNative(ref a, bind) => write!(f, "{}<{:?}>", a.name(), bind),
            XType::XUnknown => write!(f, "?"),
        }
    }
}
lazy_static!(
    pub static ref X_BOOL: Arc<XType> = Arc::new(XType::Bool);
    pub static ref X_INT: Arc<XType> = Arc::new(XType::Int);
    pub static ref X_RATIONAL: Arc<XType> = Arc::new(XType::Rational);
    pub static ref X_STRING: Arc<XType> = Arc::new(XType::String);
    pub static ref X_UNKNOWN: Arc<XType> = Arc::new(XType::XUnknown);
);

pub fn common_type<T: Iterator<Item=Result<Arc<XType>, CompilationError>>>(mut values: T) -> Result<Arc<XType>, CompilationError> {
    let ret = match values.next() {
        None => return Ok(X_UNKNOWN.clone()),
        Some(v) => v?
    };
    for res in values.by_ref() {
        let v = res?;
        if ret != v { // todo assignable?
            return Err(CompilationError::IncompatibleTypes {type0: ret.clone(), type1: v.clone()});
        }
    }
    Ok(ret.clone())
}