use crate::native_types::NativeType;
use crate::CompilationError;
use crate::Identifier;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;
use std::sync::Arc;

use crate::root_compilation_scope::Interner;

#[derive(Debug, Eq)]
pub enum XType {
    Bool,
    Int,
    Float,
    String,
    XUnknown,
    Compound(CompoundKind, Arc<XCompoundSpec>, Bind),
    XCallable(XCallableSpec),
    XFunc(XFuncSpec),
    XGeneric(Identifier),
    XNative(Box<dyn NativeType>, Vec<Arc<Self>>),
    // the actual value of this type is a struct
    Tuple(Vec<Arc<XType>>),
    XTail(Vec<Arc<XType>>),
    // this value must be resolved by turbofish
    Auto,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum CompoundKind {
    Struct,
    Union,
}

impl Display for CompoundKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct => write!(f, "struct"),
            Self::Union => write!(f, "union"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct Bind {
    bound_generics: HashMap<Identifier, Arc<XType>>,
}

impl Bind {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn from<T: Into<HashMap<Identifier, Arc<XType>>>>(bound_generics: T) -> Self {
        Self {
            bound_generics: bound_generics.into(),
        }
    }

    pub(crate) fn mix(mut self, other: &Self) -> Option<Self> {
        for (k, v) in other.bound_generics.iter() {
            if let Some(existing) = self.bound_generics.get(k) {
                let new_bind = existing.common_type(v)?;
                self.bound_generics.insert(*k, new_bind);
            } else {
                self.bound_generics.insert(*k, v.clone());
            }
        }
        Some(self)
    }

    pub(crate) fn get(&self, id: &Identifier) -> Option<&Arc<XType>> {
        self.bound_generics.get(id)
    }

    fn iter(&self) -> impl Iterator<Item = (&Identifier, &Arc<XType>)> {
        self.bound_generics.iter()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.bound_generics.is_empty()
    }
}

impl<I> FromIterator<I> for Bind
where
    HashMap<Identifier, Arc<XType>>: FromIterator<I>,
{
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        Self {
            bound_generics: FromIterator::from_iter(iter),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct XCompoundSpec {
    pub(crate) name: Identifier,
    pub(crate) generic_names: Vec<Identifier>,
    pub(crate) fields: Vec<XCompoundFieldSpec>,
    pub(crate) indices: HashMap<Identifier, usize>,
}

impl XCompoundSpec {
    pub(crate) fn new(
        name: Identifier,
        generic_names: Vec<Identifier>,
        fields: Vec<XCompoundFieldSpec>,
    ) -> Self {
        let indices =
            HashMap::from_iter(fields.iter().enumerate().map(|(i, field)| (field.name, i)));
        Self {
            name,
            generic_names,
            fields,
            indices,
        }
    }

    pub(crate) fn bind(
        &self,
        args: &[Arc<XType>],
        tail: &Arc<XType>,
        binding: &Bind,
    ) -> Option<Bind> {
        if args.len() != self.fields.len() {
            return None;
        }
        let mut ret = binding.clone();
        for (arg, param) in args.iter().zip(self.fields.iter()) {
            let t = param.type_.resolve_bind(&ret, Some(tail));
            ret = ret.mix(&t.bind_in_assignment(arg)?)?;
        }
        Some(ret)
    }

    fn generics_with_bind(&self, bind: &Bind) -> Vec<Arc<XType>> {
        let mut ret = Vec::new();
        for name in self.generic_names.iter() {
            if let Some(bound) = bind.get(name) {
                ret.push(bound.clone());
            } else {
                ret.push(Arc::new(XType::XGeneric(*name)));
            }
        }
        ret
    }

    pub(crate) fn find(
        self: &Arc<Self>,
        member_name: Identifier,
    ) -> Result<usize, CompilationError> {
        if let Some(&index) = self.indices.get(&member_name) {
            Ok(index)
        } else {
            Err(CompilationError::MemberNotFound {
                spec: self.clone(),
                name: member_name,
            })
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct XCompoundFieldSpec {
    pub name: Identifier,
    pub type_: Arc<XType>,
}

impl XCompoundFieldSpec {
    pub(crate) fn new(name: Identifier, type_: Arc<XType>) -> Self {
        Self { name, type_ }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct XCallableSpec {
    pub param_types: Vec<Arc<XType>>,
    pub return_type: Arc<XType>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct XFuncSpec {
    pub generic_params: Option<Vec<Identifier>>,
    pub(crate) params: Vec<XFuncParamSpec>,
    pub ret: Arc<XType>,
    pub(crate) short_circuit_overloads: bool,
}

impl XFuncSpec {
    pub(crate) fn new(required: &[&Arc<XType>], ret: Arc<XType>) -> Self {
        Self::new_with_optional(required, &[], ret)
    }

    pub(crate) fn new_with_optional(
        required: &[&Arc<XType>],
        optional: &[&Arc<XType>],
        ret: Arc<XType>,
    ) -> Self {
        Self {
            generic_params: None,
            params: required
                .iter()
                .map(|t| XFuncParamSpec::required((*t).clone()))
                .chain(
                    optional
                        .iter()
                        .map(|t| XFuncParamSpec::optional((*t).clone())),
                )
                .collect(),
            ret,
            short_circuit_overloads: false,
        }
    }

    pub(crate) fn generic(self, gen_params: Vec<Identifier>) -> Self {
        Self {
            generic_params: Some(gen_params),
            ..self
        }
    }

    pub(crate) fn short_circuit_overloads(self) -> Self {
        Self {
            short_circuit_overloads: true,
            ..self
        }
    }

    pub(crate) fn is_generic(&self) -> bool {
        self.generic_params.as_ref().map_or(false, |i| {
            debug_assert!(!i.is_empty());
            true
        })
    }

    fn arg_len_range(&self) -> (usize, usize) {
        let max = self.params.len();
        let min = self.params.iter().take_while(|a| a.required).count();
        (min, max)
    }

    pub(crate) fn bind(&self, args: &[Arc<XType>]) -> Option<Bind> {
        let (min, max) = self.arg_len_range();
        if args.len() < min || args.len() > max {
            return None;
        }
        let mut ret = Bind::new();
        for (arg, param) in args.iter().zip(self.params.iter()) {
            ret = ret.mix(&param.type_.bind_in_assignment(arg)?)?;
        }
        Some(ret)
    }

    pub(crate) fn rtype(&self, bind: &Bind) -> Arc<XType> {
        self.ret.clone().resolve_bind(bind, None)
    }

    pub(crate) fn xtype(&self) -> Arc<XType> {
        Arc::new(XType::XFunc(self.clone()))
    }

    pub(crate) fn skip_args(&self, n: usize) -> Self {
        Self {
            generic_params: self.generic_params.clone(),
            params: self.params.iter().skip(n).cloned().collect(),
            ret: self.ret.clone(),
            short_circuit_overloads: self.short_circuit_overloads,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct XFuncParamSpec {
    pub(crate) type_: Arc<XType>,
    pub(crate) required: bool,
}

impl XFuncParamSpec {
    fn required(t: Arc<XType>) -> Self {
        Self {
            type_: t,
            required: true,
        }
    }

    fn optional(t: Arc<XType>) -> Self {
        Self {
            type_: t,
            required: false,
        }
    }
}

impl XType {
    fn common_type(self: &Arc<Self>, other: &Arc<Self>) -> Option<Arc<Self>> {
        if self == other {
            Some(self.clone())
        } else {
            match (self.as_ref(), other.as_ref()) {
                (Self::Compound(k0, a, ref bind_a), Self::Compound(k1, b, ref bind_b)) => {
                    if a != b || k0 != k1 {
                        return None;
                    }
                    let mut bind = Bind::new();
                    for (gen_name, (gen_arg0, gen_arg1)) in a.generic_names.iter().zip(
                        a.generics_with_bind(bind_a)
                            .iter()
                            .zip(b.generics_with_bind(bind_b).iter())
                            .rev(),
                    ) {
                        bind.bound_generics
                            .insert(*gen_name, gen_arg0.common_type(gen_arg1)?);
                    }
                    Some(Self::Compound(*k0, a.clone(), bind).into())
                }
                (Self::Tuple(types1), Self::Tuple(types2)) => {
                    if types1.len() != types2.len() {
                        return None;
                    }
                    let common_types = types1
                        .iter()
                        .zip(types2.iter())
                        .map(|(t1, t2)| t1.common_type(t2))
                        .collect::<Option<Vec<_>>>()?;
                    Some(Self::Tuple(common_types).into())
                }
                (_, Self::XUnknown) => Some(self.clone()),
                (Self::XUnknown, _) => Some(other.clone()),
                (Self::XNative(a, a_bind), Self::XNative(b, b_bind)) => {
                    if a != b {
                        return None;
                    }
                    let mut bind = vec![];
                    for (a_v, b_v) in a_bind.iter().zip(b_bind.iter()) {
                        // since the types are equal we can assume they have the same keys at binding
                        if let Some(common) = a_v.common_type(b_v) {
                            bind.push(common)
                        } else {
                            return None;
                        }
                    }
                    Some(Self::XNative(a.clone(), bind).into())
                }
                _ => None,
            }
        }
    }

    pub(crate) fn bind_in_assignment(&self, other: &Arc<Self>) -> Option<Bind> {
        match (self, other.as_ref()) {
            (Self::Bool, Self::Bool) => Some(Bind::new()),
            (Self::Int, Self::Int) => Some(Bind::new()),
            (Self::Float, Self::Float) => Some(Bind::new()),
            (Self::String, Self::String) => Some(Bind::new()),
            (Self::Compound(k0, a, ref bind_a), Self::Compound(k1, b, ref bind_b)) => {
                if a != b || k0 != k1 {
                    return None;
                }
                let mut bind = Bind::new();
                for (gen_arg0, gen_arg1) in a
                    .generics_with_bind(bind_a)
                    .iter()
                    .zip(b.generics_with_bind(bind_b).iter())
                    .rev()
                {
                    bind = bind.mix(&gen_arg0.bind_in_assignment(gen_arg1)?)?;
                }
                Some(bind)
            }
            (Self::XCallable(ref a), Self::XCallable(ref b)) => {
                let mut total_binds = Bind::new();
                for (a_type, b_type) in a.param_types.iter().zip(b.param_types.iter()) {
                    if let Some(binds) = a_type.bind_in_assignment(b_type) {
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
            (Self::XFunc(ref a), Self::XFunc(ref b)) => {
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
            (Self::XCallable(ref a), Self::XFunc(ref b)) => {
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
            (Self::XNative(a, a_bind), Self::XNative(b, b_bind)) => {
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
            (Self::Tuple(types1), Self::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    return None;
                }
                let mut bind = Bind::new();
                for (a_type, b_type) in types1.iter().zip(types2.iter()) {
                    if let Some(binds) = a_type.bind_in_assignment(b_type) {
                        bind = bind.mix(&binds)?;
                    } else {
                        return None;
                    }
                }
                Some(bind)
            }
            (Self::XGeneric(ref a), Self::XGeneric(ref b)) if a == b => Some(Bind::new()),
            (_, Self::XUnknown) => Some(Bind::new()),
            (Self::XGeneric(ref a), _) => Some(Bind::from([(*a, other.clone())])),
            (Self::XUnknown, _) => Some(Bind::new()),

            _ => None,
        }
    }
    pub(crate) fn resolve_bind(
        self: &Arc<Self>,
        bind: &Bind,
        tail: Option<&Arc<Self>>,
    ) -> Arc<Self> {
        match self.as_ref() {
            Self::XNative(a, ref a_bind) => {
                let mut new_bind = HashMap::new();
                for (k, a_v) in a.generic_names().iter().zip(a_bind.iter()) {
                    new_bind.insert(k.clone(), a_v.clone().resolve_bind(bind, tail));
                }
                Self::XNative(
                    a.clone(),
                    a.generic_names()
                        .iter()
                        .map(|n| new_bind.get(n).unwrap())
                        .cloned()
                        .collect(),
                )
                .into()
            }
            Self::XGeneric(ref a) => bind.get(a).cloned().unwrap_or_else(|| self.clone()),
            Self::XTail(types) => match tail {
                None => unreachable!(),
                Some(t) => {
                    if let Self::Compound(kind, spec, _) = t.as_ref() {
                        let bind = Bind::from_iter(
                            spec.generic_names
                                .iter()
                                .zip(types.iter())
                                .map(|(n, t)| (*n, t.clone().resolve_bind(bind, None))),
                        );
                        Arc::new(Self::Compound(*kind, spec.clone(), bind))
                    } else {
                        unreachable!()
                    }
                }
            },
            Self::Tuple(types) => {
                let mut new_types = Vec::new();
                for t in types.iter() {
                    new_types.push(t.resolve_bind(bind, tail));
                }
                Self::Tuple(new_types).into()
            }
            Self::Compound(ct, spec, original_bind) => {
                let mut new_bind = Bind::default();
                for gen_name in spec.generic_names.iter(){
                    new_bind.bound_generics.insert(*gen_name, 
                        original_bind.get(gen_name)
                        .unwrap() // todo is this safe?
                        .resolve_bind(bind, tail));
                }
                Self::Compound(*ct, spec.clone(), new_bind).into()
            },
            _ => self.clone(),
        }
    }

    pub(crate) fn is_unknown(self: &Arc<Self>) -> bool {
        match self.as_ref() {
            Self::XUnknown => true,
            Self::XNative(_, types) | Self::Tuple(types) => {
                types.iter().cloned().any(|t| t.is_unknown())
            }
            Self::XCallable(spec) => {
                spec.param_types.iter().cloned().any(|t| t.is_unknown())
                    || spec.return_type.is_unknown()
            }
            Self::XFunc(spec) => {
                spec.params.iter().any(|t| t.type_.clone().is_unknown()) || spec.ret.is_unknown()
            }
            Self::Compound(.., bind) => bind.iter().any(|(_, t)| t.is_unknown()),
            Self::XTail(bind) => bind.iter().cloned().any(|t| t.is_unknown()),
            _ => false,
        }
    }

    pub(crate) fn to_string_with_interner(&self, interner: &Interner) -> String {
        match self {
            Self::Bool => "bool".to_string(),
            Self::Int => "int".to_string(),
            Self::Float => "float".to_string(),
            Self::String => "str".to_string(),
            Self::Compound(_, ref a, ref b) => {
                if a.generic_names.is_empty() {
                    interner.resolve(a.name).unwrap().to_string()
                } else {
                    format!(
                        "{}<{}>",
                        interner.resolve(a.name).unwrap(),
                        a.generic_names
                            .iter()
                            .map(|n| b
                                .get(&n.clone())
                                .map(|t| t.to_string_with_interner(interner))
                                .unwrap_or_else(|| interner.resolve(*n).unwrap().to_string()))
                            .join(", ")
                    )
                }
            }
            Self::XCallable(ref a) => format!(
                "({})->({})",
                a.param_types
                    .iter()
                    .map(|a| a.to_string_with_interner(interner))
                    .join(", "),
                a.return_type.to_string_with_interner(interner)
            ),
            Self::XFunc(ref a) => {
                let mut ret = "(".to_string();
                for (i, arg) in a.params.iter().enumerate() {
                    if i > 0 {
                        ret.push_str(", ");
                    }
                    ret.push_str(&arg.type_.to_string_with_interner(interner));
                    if !arg.required {
                        ret.push('?');
                    }
                }
                ret.push_str(")->");
                ret.push_str(&a.ret.to_string_with_interner(interner));
                ret
            }
            Self::XGeneric(ref a) => interner.resolve(*a).unwrap().to_string(),
            Self::XNative(ref a, bind) => format!(
                "{}<{}>",
                a.name(),
                bind.iter()
                    .map(|t| t.to_string_with_interner(interner))
                    .join(", ")
            ),
            Self::Tuple(ref a) => format!(
                "({})",
                a.iter()
                    .map(|t| t.to_string_with_interner(interner))
                    .join(", ")
            ),
            Self::XUnknown => "?".to_string(),
            Self::XTail(_) => "TAIL".to_string(),
            Self::Auto => unreachable!(),
        }
    }
}

impl PartialEq<Self> for XType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool, Self::Bool) => true,
            (Self::Int, Self::Int) => true,
            (Self::Float, Self::Float) => true,
            (Self::String, Self::String) => true,
            (Self::Compound(k0, ref a, ref a_b), Self::Compound(k1, ref b, ref b_b)) => {
                k0 == k1 && a.name == b.name && a_b == b_b
            }
            (Self::XCallable(ref a), Self::XCallable(ref b)) => a.eq(b),
            (Self::XFunc(ref a), Self::XFunc(ref b)) => {
                a.generic_params == b.generic_params
                    && a.params.len() == b.params.len()
                    && a.params
                        .iter()
                        .zip(b.params.iter())
                        .all(|(a, b)| a.type_.eq(&b.type_))
            }
            (Self::XCallable(ref a), Self::XFunc(ref b)) => {
                b.generic_params.is_none()
                    && a.param_types == b.params.iter().map(|p| p.type_.clone()).collect::<Vec<_>>()
                    && a.return_type.eq(&b.ret)
            }
            (Self::XFunc(_), Self::XCallable(_)) => other == self,
            (Self::XUnknown, Self::XUnknown) => true,
            (Self::XGeneric(ref a), Self::XGeneric(ref b)) => a == b,
            (Self::XNative(a, ref a_bind), Self::XNative(b, ref b_bind)) => {
                a == b && a_bind == b_bind
            }
            (Self::Tuple(ref a), Self::Tuple(ref b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| a.eq(b))
            }
            _ => false,
        }
    }
}
lazy_static! {
    pub static ref X_BOOL: Arc<XType> = Arc::new(XType::Bool);
    pub static ref X_INT: Arc<XType> = Arc::new(XType::Int);
    pub static ref X_FLOAT: Arc<XType> = Arc::new(XType::Float);
    pub static ref X_STRING: Arc<XType> = Arc::new(XType::String);
    pub static ref X_UNKNOWN: Arc<XType> = Arc::new(XType::XUnknown);
}

pub(crate) fn common_type(
    values: impl IntoIterator<Item = Result<Arc<XType>, CompilationError>>,
) -> Result<Arc<XType>, CompilationError> {
    let mut values = values.into_iter();
    let mut ret = match values.next() {
        None => return Ok(X_UNKNOWN.clone()),
        Some(v) => v?,
    };
    for res in values.by_ref() {
        let v = res?;
        ret = ret
            .common_type(&v)
            .ok_or(CompilationError::IncompatibleTypes {
                type0: ret,
                type1: v,
            })?;
    }
    Ok(ret)
}

pub(crate) struct CallbackType {
    t: Arc<XType>,
    bind: Bind,
}

impl CallbackType {
    pub(crate) fn new(t: Arc<XType>, args: &[Arc<XType>]) -> Self {
        let spec = if let XType::XFunc(spec) = t.as_ref() {
            spec
        } else {
            unreachable!()
        };
        let bind = spec.bind(args).unwrap();
        Self { t, bind }
    }

    pub(crate) fn spec(&self) -> &XFuncSpec {
        if let XType::XFunc(spec) = self.t.as_ref() {
            spec
        } else {
            unreachable!()
        }
    }

    pub(crate) fn xtype(&self) -> Arc<XType> {
        let spec = self.spec();
        Arc::new(XType::XCallable(XCallableSpec {
            param_types: spec
                .params
                .iter()
                .map(|t| t.type_.resolve_bind(&self.bind, None))
                .collect(),
            return_type: spec.ret.resolve_bind(&self.bind, None),
        }))
    }

    pub(crate) fn rtype(&self) -> Arc<XType> {
        self.spec().rtype(&self.bind)
    }
}
