use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;
use std::sync::{Arc, Weak};
use derivative::Derivative;
use string_interner::{DefaultSymbol, StringInterner};
use crate::mref::MRef;
use crate::native_types::NativeType;
use crate::xscope::Identifier;

#[derive(Debug, Eq)]
pub enum XType {
    Bool,
    Int,
    Rational,
    String,
    XUnknown,
    XStruct(MRef<XStructSpec>, Bind),
    XUnion(MRef<XUnionSpec>, Bind),
    XCallable(XCallableSpec),
    XFunc(XFuncSpec),
    XGeneric(Identifier),
    XNative(Box::<dyn NativeType>, Vec<TRef>),
}

pub type TRef = MRef<XType>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Bind {
    bound_generics: HashMap<Identifier, TRef>,
}

impl Bind {
    pub fn new() -> Self {
        Bind {
            bound_generics: HashMap::new(),
        }
    }

    pub fn from<T: Into<HashMap<Identifier, TRef>>>(bound_generics: T) -> Self {
        Bind {
            bound_generics: bound_generics.into(),
        }
    }

    pub fn from_iter<T>(bound_generics: impl IntoIterator<Item=T>) -> Self where HashMap<Identifier, TRef>: FromIterator<T> {
        Bind {
            bound_generics: HashMap::from_iter(bound_generics),
        }
    }


    pub fn mix(mut self, other: &Bind) -> Option<Self> {
        for (k, v) in other.bound_generics.iter() {
            if let Some(existing) = self.bound_generics.get(k) {
                let new_bind = existing.to_arc().bind_in_assignment(v)?;
                self = self.mix(&new_bind)?;
            } else {
                self.bound_generics.insert(k.clone(), v.clone());
            }
        }
        Some(self)
    }

    pub fn get(&self, id: &Identifier) -> Option<&TRef> {
        self.bound_generics.get(id)
    }

    pub fn iter(&self) -> impl Iterator<Item=(&Identifier, &TRef)> {
        self.bound_generics.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.bound_generics.is_empty()
    }
}


#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XStructSpec {
    pub name: Identifier,
    // this is the full qualified name
    pub generic_names: Vec<Identifier>,
    pub fields: Vec<XStructFieldSpec>,
    pub indices: BTreeMap<String, usize>,
}

pub type XUnionSpec = XStructSpec;

impl XStructSpec {
    pub fn new(name: Identifier, generic_names: Vec<Identifier>, fields: Vec<XStructFieldSpec>) -> XStructSpec {
        let mut indices = BTreeMap::new();
        for (i, field) in fields.iter().enumerate() {
            indices.insert(field.name.clone(), i);
        }
        XStructSpec {
            name,
            generic_names,
            fields,
            indices,
        }
    }

    pub fn bind(&self, args: &Vec<TRef>) -> Option<Bind> {
        if args.len() != self.fields.len() {
            return None;
        }
        let mut ret = Bind::new();
        for (arg, param) in args.into_iter().zip(self.fields.iter()) {
            ret = ret.mix(&param.type_.to_arc().bind_in_assignment(&arg)?)?;
        }
        Some(ret)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XStructFieldSpec {
    pub name: String,
    #[derivative(Hash = "ignore")]
    pub type_: TRef,
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XCallableSpec {
    #[derivative(Hash = "ignore")]
    pub param_types: Vec<TRef>,
    #[derivative(Hash = "ignore")]
    pub return_type: TRef,
}

#[derive(Clone, Debug, Eq, PartialEq, Derivative)]
#[derivative(Hash)]
pub struct XFuncSpec {
    pub generic_params: Option<Vec<DefaultSymbol>>,
    pub params: Vec<XFuncParamSpec>,
    #[derivative(Hash = "ignore")]
    pub ret: TRef,
}

impl XFuncSpec {
    pub fn arg_len_range(&self) -> (usize, usize) {
        let max = self.params.len();
        let min = self.params.iter().take_while(|a| a.required).count();
        (min, max)
    }

    pub fn bind(&self, args: &Vec<TRef>) -> Option<Bind> {
        let (min, max) = self.arg_len_range();
        if args.len() < min || args.len() > max {
            return None;
        }
        let mut ret = Bind::new();
        for (arg, param) in args.into_iter().zip(self.params.iter()) {
            ret = ret.mix(&param.type_.to_arc().bind_in_assignment(&arg)?)?;
        }
        Some(ret)
    }

    pub fn rtype(&self, bind: &Bind) -> TRef {
        self.ret.clone().resolve_bind(bind)
    }

    pub fn xtype(&self, bind: &Bind) -> TRef {
        TRef::from(XType::XFunc(
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
    pub type_: TRef,
    pub required: bool,
}

impl TRef {
    pub fn resolve_bind(&self, bind: &Bind) -> TRef {
        match self.to_arc().as_ref() {
            XType::XNative(a, ref a_bind) => {
                let mut new_bind = HashMap::new();
                for (k, a_v) in a.generic_names().iter().zip(a_bind.iter()) {
                    new_bind.insert(k.clone(), a_v.clone().resolve_bind(bind));
                }
                XType::XNative(a.clone(), a.generic_names().iter().map(|n| new_bind.get(n).unwrap()).cloned().collect()).into()
            }
            XType::XGeneric(ref a) => bind.get(a).map(|b| b.clone()).unwrap_or(self.clone()),
            _ => self.clone(),
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self.to_arc().as_ref() {
            XType::XUnknown => true,
            XType::XNative(_, types) => types.iter().cloned().any(|t| t.is_unknown()),
            XType::XCallable(spec) => spec.param_types.iter().cloned().any(|t| t.is_unknown()) || spec.return_type.is_unknown(),
            XType::XFunc(spec) => spec.params.iter().any(|t| t.type_.clone().is_unknown()) || spec.ret.is_unknown(),
            XType::XStruct(_, bind) => bind.iter().any(|(_, t)| t.is_unknown()),
            _ => false,
        }
    }

    pub fn gen_param_count(&self) -> usize {
        match self.to_arc().as_ref() {
            XType::XNative(n, _) => n.generic_names().len(),
            XType::XStruct(spec, ..) | XType::XUnion(spec, ..) => spec.to_arc().generic_names.len(),
            _ => 0,
        }
    }

    pub fn bind(&self, generic_params: Vec<Self>) -> Result<Self, String> {
        let with_compound = |spec: &MRef<XStructSpec>| -> Result<Bind, String>{
            let spec = spec.to_arc();
            if generic_params.len() != spec.generic_names.len() {
                unreachable!();
            }
            // we only bind with pure types, so the current bind is always just generics
            Ok(Bind::from_iter(spec.generic_names.iter().zip(generic_params.iter())
                .map(|(n, t)| (n.clone(), t.clone()))))
        };

        if generic_params.is_empty() {
            return Ok(self.clone());
        }
        match self.to_arc().as_ref() {
            XType::XStruct(spec, ..) => Ok(Self::from(XType::XStruct(spec.clone(), with_compound(spec)?))),
            XType::XUnion(spec, ..) => Ok(Self::from(XType::XUnion(spec.clone(), with_compound(spec)?))),
            XType::XNative(a, ..) => {
                if generic_params.len() != a.generic_names().len() {
                    unreachable!();
                }
                // we only bind with pure types, so the current bind is always just generics
                Ok(Self::from(XType::XNative(a.clone(), generic_params.clone())))
            }
            _ => unreachable!(),
        }
    }
}

impl XType {
    pub fn generic_from_name(name: &'static str, interner: &mut StringInterner) -> TRef {
        TRef::from(XType::XGeneric(interner.get_or_intern_static(name)))
    }

    pub fn bind_in_assignment(&self, other: &TRef) -> Option<Bind> {
        println!("!!! I.0 {:?};{:?}", self, other);
        match (self, other.to_arc().as_ref()) {
            (XType::Bool, XType::Bool) => Some(Bind::new()),
            (XType::Int, XType::Int) => Some(Bind::new()),
            (XType::Rational, XType::Rational) => Some(Bind::new()),
            (XType::String, XType::String) => Some(Bind::new()),
            (XType::XStruct(a, ref bind_a), XType::XStruct(b, ref bind_b)) |
            (XType::XUnion(a, ref bind_a), XType::XUnion(b, ref bind_b)) => {
                println!("!!! T.0: {:?} {:?}", a, b);
                if a != b {
                    return None;
                }
                if bind_a == bind_b {
                    return Some(bind_a.clone());
                }
                println!("!!! T.1");
                let mut bind = Bind::new();
                for p_type in a.to_arc().fields.iter().map(|f| f.type_.clone()) {
                    println!("!!! T.1.2 {:?};{:?};{:?};{:?};{:?}", p_type, p_type.clone().resolve_bind(bind_a), p_type.resolve_bind(bind_b), bind_a, bind_b);
                    if let Some(binds) = p_type.clone().resolve_bind(bind_a).to_arc().bind_in_assignment(&p_type.resolve_bind(bind_b)) {
                        println!("!!! T.1.2.1 {:?}", binds);
                        bind = bind.mix(&binds)?;
                        println!("!!! T.1.2.1.2 {:?}", bind);
                    } else {
                        println!("!!! T.1.2.2");
                        return None;
                    }
                }
                Some(bind)
            }
            (XType::XCallable(ref a), XType::XCallable(ref b)) => {
                let mut total_binds = Bind::new();
                for (a_type, b_type) in a.param_types.iter().zip(b.param_types.iter()) {
                    if let Some(binds) = a_type.to_arc().bind_in_assignment(&b_type) {
                        total_binds = total_binds.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.return_type.to_arc().bind_in_assignment(&b.return_type) {
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
                    if let Some(binds) = a_arg.type_.to_arc().bind_in_assignment(&b_arg.type_) {
                        total_binds = total_binds.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.ret.to_arc().bind_in_assignment(&b.ret) {
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
                    if let Some(binds) = a_type.to_arc().bind_in_assignment(&b_arg.type_) {
                        total_binds = total_binds.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                if let Some(binds) = a.return_type.to_arc().bind_in_assignment(&b.ret) {
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
                    if let Some(binds) = a_v.to_arc().bind_in_assignment(b_v) {
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
                println!("!!! G.0");
                Some(Bind::new())
            }
            (XType::XGeneric(ref a), _) => {
                println!("!!! G.1 {:?};{:?}", self, other);
                Some(Bind::from([
                    (a.clone(), other.clone()),
                ]))
            }

            _ => {
                None
            }
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
            (XType::XStruct(ref a, ref a_b), XType::XStruct(ref b, ref b_b)) => a.to_arc().name == b.to_arc().name && a_b == b_b,
            (XType::XUnion(ref a, ref a_b), XType::XUnion(ref b, ref b_b)) => a.to_arc().name == b.to_arc().name && a_b == b_b,
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
            XType::XStruct(ref a, ref b) => write!(f, "{:?}<{:?}>", a.to_arc().name, b),
            XType::XUnion(ref a, ref b) => write!(f, "{:?}<{:?}>", a.to_arc().name, b),
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
    pub static ref X_BOOL: TRef = TRef::from(XType::Bool);
    pub static ref X_INT: TRef = TRef::from(XType::Int);
    pub static ref X_RATIONAL: TRef = TRef::from(XType::Rational);
    pub static ref X_STRING: TRef = TRef::from(XType::String);
    pub static ref X_UNKNOWN: TRef = TRef::from(XType::XUnknown);
);

pub fn common_type<T: Iterator<Item=Result<TRef, String>>>(mut values: T) -> Result<TRef, String> {
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