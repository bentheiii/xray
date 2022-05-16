use std::collections::{BTreeMap, HashMap};
use std::fmt::{Display, Formatter};
use std::net::Incoming;
use std::sync::Arc;
use derivative::Derivative;
use string_interner::{DefaultSymbol, StringInterner};
use crate::native_types::NativeType;
use crate::xscope::Identifier;

#[derive(Debug, Eq)]
pub enum XType {
    Bool,
    Int,
    Rational,
    String,
    XUnknown,
    XStruct(XStructSpec, Bind),
    XCallable(XCallableSpec),
    XFunc(XFuncSpec),
    XGeneric(Identifier),
    XNative(Box::<dyn NativeType>, Vec<Arc<XType>>),
}

pub type Bind = HashMap<Identifier, Arc<XType>>;



#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XStructSpec {
    pub name: Identifier,
    // this is the full qualified name
    pub fields: Vec<XStructFieldSpec>,
    pub indices: BTreeMap<String, usize>,
}

impl XStructSpec {
    pub fn new(name: Identifier, fields: Vec<XStructFieldSpec>) -> XStructSpec {
        let mut indices = BTreeMap::new();
        for (i, field) in fields.iter().enumerate() {
            indices.insert(field.name.clone(), i);
        }
        XStructSpec {
            name,
            fields,
            indices,
        }
    }

    pub fn bind(&self, args: Vec<Arc<XType>>) -> Option<Bind> {
        if args.len() != self.fields.len() {
            return None;
        }
        let mut ret = HashMap::new();
        for (arg, param) in args.into_iter().zip(self.fields.iter()) {
            ret = mix_binds(&mut ret, param.type_.bind_in_assignment(&arg)?)?;
        }
        Some(ret)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XStructFieldSpec {
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

    pub fn bind(&self, args: Vec<Arc<XType>>) -> Option<Bind> {
        let (min, max) = self.arg_len_range();
        if args.len() < min || args.len() > max {
            return None;
        }
        let mut ret = HashMap::new();
        for (arg, param) in args.into_iter().zip(self.params.iter()) {
            ret = mix_binds(&mut ret, param.type_.bind_in_assignment(&arg)?)?;
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

pub fn mix_binds(binds1: &mut Bind, binds2: Bind) -> Option<Bind> {
    let mut binds = binds1.clone();
    for (k, v) in binds2.iter() {
        if binds.contains_key(k) {
            if *binds[k] != **v {
                return None;
            }
        } else {
            binds.insert(k.clone(), v.clone());
        }
    }
    Some(binds)
}

impl XType {
    pub fn generic_from_name(name: &'static str, interner: &mut StringInterner) -> Arc<XType> {
        Arc::new(XType::XGeneric(interner.get_or_intern_static(name)))
    }

    pub fn bind_in_assignment(&self, other: &Arc<XType>) -> Option<Bind> {
        match (self, other.as_ref()) {
            (XType::Bool, XType::Bool) => Some(HashMap::new()),
            (XType::Int, XType::Int) => Some(HashMap::new()),
            (XType::Rational, XType::Rational) => Some(HashMap::new()),
            (XType::String, XType::String) => Some(HashMap::new()),
            (XType::XStruct(a, ref bind_a), XType::XStruct(b, ref bind_b)) => {
                if a != b {
                    return None;
                }
                let mut bind = HashMap::new();
                for p_type in a.fields.iter().map(|f| f.type_.clone()) {
                    if let Some(binds) = p_type.clone().resolve_bind(bind_a).bind_in_assignment(&p_type.resolve_bind(bind_b)) {
                        bind = mix_binds(&mut bind, binds)?;
                    } else {
                        return None;
                    }
                }
                Some(bind)
            },
            (XType::XCallable(ref a), XType::XCallable(ref b)) => {
                let mut total_binds = HashMap::new();
                for (a_type, b_type) in a.param_types.iter().zip(b.param_types.iter()) {
                    if let Some(binds) = a_type.bind_in_assignment(&b_type) {
                        total_binds = mix_binds(&mut total_binds, binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.return_type.bind_in_assignment(&b.return_type) {
                    total_binds = mix_binds(&mut total_binds, binds)?;
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
                let mut total_binds = HashMap::new();
                for (a_arg, b_arg) in a.params.iter().zip(b.params.iter()) {
                    if let Some(binds) = a_arg.type_.bind_in_assignment(&b_arg.type_) {
                        total_binds = mix_binds(&mut total_binds, binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.ret.bind_in_assignment(&b.ret) {
                    total_binds = mix_binds(&mut total_binds, binds)?;
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
                let mut total_binds = HashMap::new();
                for (a_type, b_arg) in a.param_types.iter().zip(b.params.iter()) {
                    if let Some(binds) = a_type.bind_in_assignment(&b_arg.type_) {
                        total_binds = mix_binds(&mut total_binds, binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.return_type.bind_in_assignment(&b.ret) {
                    total_binds = mix_binds(&mut total_binds, binds)?;
                } else {
                    return None;
                }
                Some(total_binds)
            }
            (XType::XNative(a, a_bind), XType::XNative(b, b_bind)) => {
                if a != b {
                    return None;
                }
                let mut bind = HashMap::new();
                for (a_v, b_v) in a_bind.iter().zip(b_bind.iter()) {
                    // since the types are equal we can assume they have the same keys at binding
                    if let Some(binds) = a_v.bind_in_assignment(b_v) {
                        bind = mix_binds(&mut bind, binds)?;
                    } else {
                        return None;
                    }
                }
                Some(bind)
            }
            (_, XType::XUnknown) => Some(HashMap::new()),
            (XType::XGeneric(ref a), XType::XGeneric(ref b)) if a == b => {
                Some(HashMap::new())
            },
            (XType::XGeneric(ref a), _) => {
                Some(HashMap::from([
                    (a.clone(), other.clone()),
                ]))
            },

            _ => None,
        }
    }
    pub fn resolve_bind(self: Arc<XType>, bind: &Bind) -> Arc<XType> {
        match self.as_ref() {
            XType::XNative(a, ref a_bind) => {
                let mut new_bind = HashMap::new();
                for (k, a_v) in a.generic_names().iter().zip(a_bind.iter()) {
                    new_bind.insert(k.clone(), a_v.clone().resolve_bind(bind));
                }
                XType::XNative(a.clone(), a.generic_names().iter().map(|n| new_bind.get(n).unwrap().clone()).collect()).into()
            },
            XType::XGeneric(ref a) => bind.get(a).map(|b| b.clone()).unwrap_or(self.clone()),
            _ => self,
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
            (XType::XStruct(ref a, ref a_b), XType::XStruct(ref b, ref b_b)) => a.name == b.name && a_b == b_b,
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
            XType::XStruct(ref a, ref b) => write!(f, "{:?}<{:?}>", a.name, b),
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

pub fn common_type<T: Iterator<Item=Result<Arc<XType>, String>>>(mut values: T) -> Result<Arc<XType>, String> {
    let ret = match values.next() {
        None => return Ok(X_UNKNOWN.clone()),
        Some(v) => v?
    };
    for res in values.by_ref() {
        let v = res?;
        if ret != v {
            return Err(format!("incompatible types {:?} and {:?}", ret, v));
        }
    }
    Ok(ret.clone())
}