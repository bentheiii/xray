use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::{Display, Formatter};
use derivative::Derivative;

#[derive(Clone, Debug, Eq, Derivative)]
#[derivative(Hash)]
pub enum XType {
    Bool,
    Int,
    Rational,
    String,
    XUnknown,
    XSeq(Box<XType>),
    XSet(Box<XType>),
    XMap(Box<XType>, Box<XType>),
    XStruct(XStructSpec,
            #[derivative(Hash="ignore")]
            HashMap<String, XType>
    ),
    XFunc(XFuncSpec),
    XGeneric(String),
}

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XStructSpec {
    pub name: String,
    // this is the full qualified name
    pub fields: Vec<XStructFieldSpec>,
    pub indices: BTreeMap<String, usize>,
}

impl XStructSpec {
    pub fn new(name: String, fields: Vec<XStructFieldSpec>) -> XStructSpec {
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

    pub fn bind(&self, args: Vec<XType>) -> Option<HashMap<String, XType>> {
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

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XStructFieldSpec {
    pub name: String,
    pub type_: Box<XType>,
}

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XFuncSpec {
    pub generic_params: Option<Vec<String>>,
    pub params: Vec<XFuncParamSpec>,
    pub ret: Box<XType>,
}

impl XFuncSpec {
    pub fn arg_len_range(&self) -> (usize, usize) {
        let max = self.params.len();
        let min = self.params.iter().take_while(|a| a.required).count();
        (min, max)
    }

    pub fn bind(&self, args: Vec<XType>) -> Option<HashMap<String, XType>> {
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

    pub fn rtype(&self, bind: &HashMap<String, XType>) -> XType {
        return self.ret.resolve_bind(bind);
    }

    pub fn xtype(&self, bind: &HashMap<String, XType>) -> XType {
        XType::XFunc(
            XFuncSpec {
                generic_params: None,
                params: self.params.iter().map(|p| {
                    XFuncParamSpec {
                        type_: Box::new(p.type_.resolve_bind(bind)),
                        required: p.required,
                    }
                }).collect(),
                ret: Box::new(self.ret.resolve_bind(bind)),
            }
        )
    }
}

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct XFuncParamSpec {
    pub type_: Box<XType>,
    pub required: bool,
}

fn mix_binds(binds1: &mut HashMap<String, XType>, binds2: HashMap<String, XType>) -> Option<HashMap<String, XType>> {
    let mut binds = binds1.clone();
    for (k, v) in binds2.iter() {
        if binds.contains_key(k) {
            if &binds[k] != v {
                return None;
            }
        } else {
            binds.insert(k.clone(), v.clone());
        }
    }
    Some(binds)
}

impl XType {
    pub fn bind_in_assignment(&self, other: &XType) -> Option<HashMap<String, XType>> {
        match (self, other) {
            (XType::Bool, XType::Bool) => Some(HashMap::new()),
            (XType::Int, XType::Int) => Some(HashMap::new()),
            (XType::Rational, XType::Rational) => Some(HashMap::new()),
            (XType::String, XType::String) => Some(HashMap::new()),
            (XType::XSeq(ref a), XType::XSeq(ref b)) => a.bind_in_assignment(b),
            (XType::XSet(ref a), XType::XSet(ref b)) => a.bind_in_assignment(b),
            (XType::XMap(ref a, ref b), XType::XMap(ref c, ref d)) => a.bind_in_assignment(c).and_then(|mut ns| {
                if let Some(binds) = b.bind_in_assignment(d) {
                    mix_binds(&mut ns, binds)
                } else {
                    None
                }
            }),
            (XType::XStruct(a, ref bind_a), XType::XStruct(b, ref bind_b)) => {
                let mut bind = HashMap::new();
                for p_type in a.fields.iter().map(|f| f.type_.as_ref()) {
                    if let Some(binds) = p_type.resolve_bind(bind_a).bind_in_assignment(&p_type.resolve_bind(bind_b)) {
                        bind = mix_binds(&mut bind, binds)?;
                    } else {
                        return None;
                    }
                }
                Some(bind)
            },
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
            (_, XType::XUnknown) => Some(HashMap::new()),
            (XType::XGeneric(ref a), other) => {
                Some(HashMap::from([
                    (a.clone(), other.clone()),
                ]))
            },

            _ => None,
        }
    }
    pub fn resolve_bind(&self, bind: &HashMap<String, XType>) -> XType {
        match self {
            XType::XSeq(ref a) => XType::XSeq(Box::new(a.resolve_bind(bind))),
            XType::XSet(ref a) => XType::XSet(Box::new(a.resolve_bind(bind))),
            XType::XMap(ref a, ref b) => XType::XMap(Box::new(a.resolve_bind(bind)), Box::new(b.resolve_bind(bind))),
            XType::XGeneric(ref a) => bind.get(a).map(|b| b.clone()).unwrap_or(self.clone()),
            other => other.clone(),
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
            (XType::XSeq(ref a), XType::XSeq(ref b)) => a.eq(b),
            (XType::XSet(ref a), XType::XSet(ref b)) => a.eq(b),
            (XType::XMap(ref a, ref b), XType::XMap(ref c, ref d)) => a.eq(c) && b.eq(d),
            (XType::XStruct(ref a, ref a_b), XType::XStruct(ref b, ref b_b)) => a.name == b.name && a_b == b_b,
            (XType::XFunc(ref a), XType::XFunc(ref b)) => a.generic_params == b.generic_params && a.params.len() == b.params.len() && a.params.iter().zip(b.params.iter()).all(|(a, b)| a.type_.eq(&b.type_)),
            (XType::XUnknown, XType::XUnknown) => true,
            (XType::XGeneric(ref a), XType::XGeneric(ref b)) => a == b,
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
            XType::XSeq(ref a) => write!(f, "Array<{}>", a),
            XType::XSet(ref a) => write!(f, "Set<{}>", a),
            XType::XMap(ref a, ref b) => write!(f, "Map<{},{}>", a, b),
            XType::XStruct(ref a, ref b) => write!(f, "{}<{:?}>", a.name, b),
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
            XType::XGeneric(ref a) => write!(f, "{}", a),
            XType::XUnknown => write!(f, "?"),
        }
    }
}

pub const X_BOOL: XType = XType::Bool;
pub const X_INT: XType = XType::Int;
pub const X_RATIONAL: XType = XType::Rational;
pub const X_STRING: XType = XType::String;
pub const X_UNKNOWN: XType = XType::XUnknown;

pub fn common_type<T: Iterator<Item=Result<XType, String>>>(mut values: T) -> Result<XType, String> {
    let ret = match values.next() {
        None => return Ok(X_UNKNOWN),
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