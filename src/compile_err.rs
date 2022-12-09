use crate::parser::Rule;
use crate::root_compilation_scope::XCompilationScopeItem;
use crate::xexpr::XExpr;
use crate::xtype::CompoundKind;
use crate::Identifier;
use crate::{XCompoundSpec, XType};
use derivative::Derivative;
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Position;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::sync::Arc;
use string_interner::StringInterner;
use strum::IntoStaticStr;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct TracedCompilationError<W: Write + 'static>(
    CompilationError<W>,
    ((usize, usize), usize),
    ((usize, usize), usize),
);

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum CompilationError<W: Write + 'static> {
    VariableTypeMismatch {
        variable_name: Identifier,
        expected_type: Arc<XType>,
        actual_type: Arc<XType>,
    },
    RequiredParamsAfterOptionalParams {
        function_name: Option<Identifier>,
        param_name: Identifier,
    },
    DefaultEvaluationError {
        function_name: Option<Identifier>,
        // None for lambda function
        param_name: Identifier,
        error: String, // todo fix when we have proper eval error handling
    },
    FunctionOutputTypeMismatch {
        function_name: Identifier,
        expected_type: Arc<XType>,
        actual_type: Arc<XType>,
    },
    TypeNotFound {
        name: String,
    },
    GenericParamCountMismatch {
        type_name: String,
        expected_count: usize,
        actual_count: usize,
    },
    ValueIsNotType {
        name: Identifier,
        item: XCompilationScopeItem<W>,
    },
    PairNotType,
    NameAlreadyDefined {
        name: Identifier,
        other: XCompilationScopeItem<W>,
    },
    AmbiguousOverload {
        name: Identifier,
        is_generic: bool,
        items: usize, // todo improve
        param_types: Option<Vec<Arc<XType>>>,
    },
    NoOverload {
        name: Identifier,
        param_types: Option<Vec<Arc<XType>>>,
        dynamic_failures: Vec<String>, // todo change to real errors
    },
    VariantConstructorOneArg,
    VariantConstructorTypeArgMismatch {
        union_name: Identifier,
        variant_name: String,
        expected_type: Arc<XType>,
        actual_type: Arc<XType>,
    },
    StructParamsLengthMismatch {
        struct_name: Identifier,
        expected_count: usize,
        actual_count: usize,
    },
    StructFieldTypeMismatch {
        struct_name: Identifier,
        expected_types: Vec<Arc<XType>>,
        actual_types: Vec<Arc<XType>>,
    },
    NonFunctionSpecialization {
        name: Identifier,
        item: XCompilationScopeItem<W>,
    },
    SpecializedFunctionTypeMismatch {
        name: Identifier,
        idx: usize,
        expected_type: Arc<XType>,
        actual_type: Arc<XType>,
    },
    FunctionNotFound {
        name: Identifier,
    },
    MemberNotFound {
        spec: Arc<XCompoundSpec>,
        name: String,
    },
    NonCompoundMemberAccess {
        xtype: Arc<XType>,
    },
    NonItemTupleAccess {
        member: String,
    },
    TupleIndexOutOfBounds {
        tuple_type: Arc<XType>,
        index: usize,
        max: usize,
    },
    ValueNotFound {
        name: Identifier,
    },
    TypeAsVariable {
        name: Identifier,
    },
    GenericFunctionAsVariable {
        name: Identifier,
    },
    OverloadedFunctionAsVariable {
        name: Identifier,
    },
    DynamicFunctionAsVariable {
        name: Identifier,
    },
    IncompatibleTypes {
        type0: Arc<XType>,
        type1: Arc<XType>,
    },
    NotAFunction {
        type_: Arc<XType>,
    },
    NotACompound {
        type_: Arc<XType>,
    },
    InvalidAutoLocation,
    AutoSpecializationWithoutCall,
}

trait Resolve {
    type Output;
    fn resolve(&self, interner: &StringInterner) -> Self::Output;
}

impl Resolve for Arc<XType> {
    type Output = ResolvedType;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        ResolvedType(self.to_string_with_interner(interner), self.clone())
    }
}

impl Resolve for Identifier {
    type Output = String;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        interner.resolve(*self).unwrap().to_string()
    }
}

impl Resolve for XCompoundSpec {
    type Output = String;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        interner.resolve(self.name).unwrap().to_string()
    }
}

impl<W: Write + 'static> Resolve for XCompilationScopeItem<W> {
    type Output = ResolvedCompilationScopeItem;

    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        match self {
            Self::Value(_, t) => ResolvedCompilationScopeItem::Value(t.resolve(interner)),
            Self::NativeType(..) => ResolvedCompilationScopeItem::NativeType,
            Self::Compound(k, ..) => ResolvedCompilationScopeItem::Compound(*k),
            Self::Overload(overloads) => ResolvedCompilationScopeItem::Overloads(overloads.len()),
        }
    }
}

impl<T: Resolve> Resolve for Arc<T> {
    type Output = T::Output;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        self.as_ref().resolve(interner)
    }
}

impl<T: Resolve + Clone> Resolve for Option<T> {
    type Output = Option<T::Output>;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        self.clone().map(|i| i.resolve(interner))
    }
}

impl<T: Resolve + Clone> Resolve for Vec<T> {
    type Output = Vec<T::Output>;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        self.iter().map(|i| i.resolve(interner)).collect()
    }
}

impl<W: Write + 'static> Resolve for Vec<XExpr<W>> {
    type Output = Self;
    fn resolve(&self, _interner: &StringInterner) -> Self::Output {
        self.clone()
    }
}

macro_rules! trivial_resolve {
    ($type_: ty) => {
        impl Resolve for $type_ {
            type Output = Self;
            fn resolve(&self, _interner: &StringInterner) -> Self::Output {
                self.clone()
            }
        }
    };
}

trivial_resolve!(String);
trivial_resolve!(bool);
trivial_resolve!(usize);

macro_rules! resolve_variants {
    ($self: ident, $interner: expr, $($variant:ident {$($part:ident),*  $(,)?}),+ $(,)?) => {{
        match $self{
            $(
            Self::$variant{$($part),*}=>ResolvedCompilationError::$variant{$($part: $part.resolve($interner)),*}
            ),+
        }
    }}
}

impl<W: Write + 'static> Resolve for CompilationError<W> {
    type Output = ResolvedCompilationError;
    fn resolve(&self, interner: &StringInterner) -> Self::Output {
        resolve_variants!(
            self,
            interner,
            VariableTypeMismatch {
                variable_name,
                expected_type,
                actual_type
            },
            RequiredParamsAfterOptionalParams {
                function_name,
                param_name
            },
            DefaultEvaluationError {
                function_name,
                param_name,
                error,
            },
            FunctionOutputTypeMismatch {
                function_name,
                expected_type,
                actual_type,
            },
            TypeNotFound { name },
            GenericParamCountMismatch {
                type_name,
                expected_count,
                actual_count,
            },
            ValueIsNotType { name, item },
            PairNotType {},
            NameAlreadyDefined { name, other },
            AmbiguousOverload {
                name,
                is_generic,
                items,
                param_types,
            },
            NoOverload {
                name,
                param_types,
                dynamic_failures,
            },
            VariantConstructorOneArg {},
            VariantConstructorTypeArgMismatch {
                union_name,
                variant_name,
                expected_type,
                actual_type,
            },
            StructParamsLengthMismatch {
                struct_name,
                expected_count,
                actual_count,
            },
            StructFieldTypeMismatch {
                struct_name,
                expected_types,
                actual_types,
            },
            NonFunctionSpecialization { name, item },
            SpecializedFunctionTypeMismatch {
                name,
                idx,
                expected_type,
                actual_type,
            },
            FunctionNotFound { name },
            MemberNotFound { spec, name },
            NonCompoundMemberAccess { xtype },
            NonItemTupleAccess { member },
            TupleIndexOutOfBounds {
                tuple_type,
                index,
                max,
            },
            ValueNotFound { name },
            TypeAsVariable { name },
            GenericFunctionAsVariable { name },
            OverloadedFunctionAsVariable { name },
            IncompatibleTypes { type0, type1 },
            NotAFunction { type_ },
            NotACompound { type_ },
            DynamicFunctionAsVariable { name },
            InvalidAutoLocation {},
            AutoSpecializationWithoutCall {},
        )
    }
}

impl<W: Write + 'static> CompilationError<W> {
    pub(crate) fn trace(self, input: &Pair<Rule>) -> TracedCompilationError<W> {
        fn pos_to_coors(pos: &Position) -> ((usize, usize), usize) {
            (pos.line_col(), pos.pos())
        }
        let start = input.as_span().start_pos();
        let end = input.as_span().end_pos();
        TracedCompilationError(self, pos_to_coors(&start), pos_to_coors(&end))
    }
}

impl<W: Write + 'static> TracedCompilationError<W> {
    pub(crate) fn resolve_with_input(
        self,
        interner: &StringInterner,
        input: &str,
    ) -> ResolvedTracedCompilationError {
        let Self(err, ((start_line, _), start_pos), (_, end_pos)) = self;
        ResolvedTracedCompilationError::Compilation(
            err.resolve(interner),
            start_line,
            input[start_pos..end_pos].to_string(),
        )
    }
}

pub struct ResolvedType(String, Arc<XType>);

impl Display for ResolvedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub enum ResolvedCompilationScopeItem {
    Value(ResolvedType),
    NativeType,
    Compound(CompoundKind),
    Overloads(usize),
}

impl Display for ResolvedCompilationScopeItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(t) => write!(f, "variable of type {t}"),
            Self::NativeType => write!(f, "native type"),
            Self::Compound(k) => write!(f, "{k}"),
            Self::Overloads(1) => write!(f, "function"),
            Self::Overloads(len) => write!(f, "{len} overloaded functions"),
        }
    }
}

#[derive(IntoStaticStr)]
pub enum ResolvedCompilationError {
    VariableTypeMismatch {
        variable_name: String,
        expected_type: ResolvedType,
        actual_type: ResolvedType,
    },
    RequiredParamsAfterOptionalParams {
        function_name: Option<String>,
        param_name: String,
    },
    DefaultEvaluationError {
        function_name: Option<String>,
        // None for lambda function
        param_name: String,
        error: String, // todo fix when we have proper eval error handling
    },
    FunctionOutputTypeMismatch {
        function_name: String,
        expected_type: ResolvedType,
        actual_type: ResolvedType,
    },
    TypeNotFound {
        name: String,
    },
    GenericParamCountMismatch {
        type_name: String,
        expected_count: usize,
        actual_count: usize,
    },
    ValueIsNotType {
        name: String,
        item: ResolvedCompilationScopeItem,
    },
    PairNotType,
    NameAlreadyDefined {
        name: String,
        other: ResolvedCompilationScopeItem,
    },
    AmbiguousOverload {
        name: String,
        is_generic: bool,
        items: usize,
        param_types: Option<Vec<ResolvedType>>,
    },
    NoOverload {
        name: String,
        param_types: Option<Vec<ResolvedType>>,
        dynamic_failures: Vec<String>, // todo change to real errors
    },
    VariantConstructorOneArg,
    VariantConstructorTypeArgMismatch {
        union_name: String,
        variant_name: String,
        expected_type: ResolvedType,
        actual_type: ResolvedType,
    },
    StructParamsLengthMismatch {
        struct_name: String,
        expected_count: usize,
        actual_count: usize,
    },
    StructFieldTypeMismatch {
        struct_name: String,
        expected_types: Vec<ResolvedType>,
        actual_types: Vec<ResolvedType>,
    },
    NonFunctionSpecialization {
        name: String,
        item: ResolvedCompilationScopeItem,
    },
    SpecializedFunctionTypeMismatch {
        name: String,
        idx: usize,
        expected_type: ResolvedType,
        actual_type: ResolvedType,
    },
    FunctionNotFound {
        name: String,
    },
    MemberNotFound {
        spec: String,
        name: String,
    },
    NonCompoundMemberAccess {
        xtype: ResolvedType,
    },
    NonItemTupleAccess {
        member: String,
    },
    TupleIndexOutOfBounds {
        tuple_type: ResolvedType,
        index: usize,
        max: usize,
    },
    ValueNotFound {
        name: String,
    },
    TypeAsVariable {
        name: String,
    },
    GenericFunctionAsVariable {
        name: String,
    },
    OverloadedFunctionAsVariable {
        name: String,
    },
    IncompatibleTypes {
        type0: ResolvedType,
        type1: ResolvedType,
    },
    NotAFunction {
        type_: ResolvedType,
    },
    NotACompound {
        type_: ResolvedType,
    },
    DynamicFunctionAsVariable {
        name: String,
    },
    InvalidAutoLocation,
    AutoSpecializationWithoutCall,
}

impl Display for ResolvedCompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableTypeMismatch {
                variable_name,
                expected_type,
                actual_type,
            } => {
                write!(
                    f,
                    "Variable {variable_name} has type {actual_type}, but expected {expected_type}"
                )
            }
            Self::RequiredParamsAfterOptionalParams {
                function_name,
                param_name,
            } => {
                write!(
                    f,
                    "Required parameter {} after optional parameter in function {}",
                    param_name,
                    function_name.as_deref().unwrap_or("<lambda>")
                )
            }
            Self::DefaultEvaluationError {
                function_name,
                param_name,
                error,
            } => {
                write!(
                    f,
                    "Error evaluating default value for parameter {} in function {}: {}",
                    param_name,
                    function_name.as_deref().unwrap_or("<lambda>"),
                    error
                )
            }
            Self::FunctionOutputTypeMismatch {
                function_name,
                expected_type,
                actual_type,
            } => {
                write!(
                    f,
                    "Function {function_name} has output type {actual_type}, but expected {expected_type}"
                )
            }
            Self::TypeNotFound { name } => {
                write!(f, "Type {name} not found")
            }
            Self::GenericParamCountMismatch {
                type_name,
                expected_count,
                actual_count,
            } => {
                write!(
                    f,
                    "Type {type_name} has {actual_count} generic parameters, but expected {expected_count}"
                )
            }
            Self::ValueIsNotType { name, item } => {
                write!(f, "{name} is not of type (found {item})",)
            }
            Self::PairNotType => {
                write!(f, "Expression cannot be interpreted as a type",)
            }
            Self::NameAlreadyDefined { name, other } => {
                write!(f, "Name {name} is already defined as {other}")
            }
            Self::AmbiguousOverload {
                name,
                is_generic,
                items,
                param_types,
            } => {
                write!(
                    f,
                    "Overload{} for {} is ambiguous{}: {:?}",
                    if *is_generic { " (generic)" } else { "" },
                    name,
                    param_types.as_ref().map_or_else(
                        || "".to_string(),
                        |types| format!(" for param types {}", types.iter().format(","))
                    ),
                    items
                )
            }
            Self::NoOverload {
                name,
                param_types,
                dynamic_failures,
            } => {
                write!(
                    f,
                    "No overload for {} found{}: {}",
                    name,
                    param_types.as_ref().map_or_else(
                        || "".to_string(),
                        |types| format!(" for param types {}", types.iter().format(","))
                    ),
                    if dynamic_failures.is_empty() {
                        "".to_string()
                    } else {
                        " dynamic failures: ".to_owned() + &dynamic_failures.join(", ")
                    },
                )
            }
            Self::VariantConstructorOneArg => {
                write!(f, "Variant constructors must have exactly one argument")
            }
            Self::VariantConstructorTypeArgMismatch {
                union_name,
                variant_name,
                expected_type,
                actual_type,
            } => {
                write!(
                    f,
                    "Variant {variant_name} of union {union_name} has type {actual_type}, but expected {expected_type}"
                )
            }
            Self::StructParamsLengthMismatch {
                struct_name,
                expected_count,
                actual_count,
            } => {
                write!(
                    f,
                    "Struct {struct_name} has {actual_count} parameters, but expected {expected_count}"
                )
            }
            Self::StructFieldTypeMismatch {
                struct_name,
                expected_types,
                actual_types,
            } => {
                write!(
                    f,
                    "Struct {} has parameters of types [{:?}], but expected [{:?}]",
                    struct_name,
                    actual_types.iter().join(", "),
                    expected_types.iter().join(", ")
                )
            }
            Self::NonFunctionSpecialization { name, item } => {
                write!(f, "Cannot specialize non-function {name} (found {item})",)
            }
            Self::SpecializedFunctionTypeMismatch {
                name,
                idx,
                expected_type,
                actual_type,
            } => {
                write!(
                    f,
                    "Specialized argument at index {idx} of function {name} has type {actual_type}, but expected {expected_type}",
                )
            }
            Self::FunctionNotFound { name } => {
                write!(f, "Function {name} not found")
            }
            Self::MemberNotFound { spec, name } => {
                write!(f, "Member {name} not found in compound {spec}")
            }
            Self::NonCompoundMemberAccess { xtype } => {
                write!(f, "Cannot access member of non-compound type {xtype}")
            }
            Self::NonItemTupleAccess { member } => {
                write!(
                    f,
                    "Member access to tuple must be of the for \"item<positive number>\", got {member:?}"
                )
            }
            Self::TupleIndexOutOfBounds {
                tuple_type,
                index,
                max: max_index,
            } => {
                write!(
                    f,
                    "Tuple index {index} out of bounds for tuple {tuple_type} of size {max_index}"
                )
            }
            Self::ValueNotFound { name } => {
                write!(f, "Value {name} not found")
            }
            Self::TypeAsVariable { name } => {
                write!(f, "Cannot use type {name} as variable")
            }
            Self::GenericFunctionAsVariable { name } => {
                write!(f, "Cannot use generic function {name} as variable")
            }
            Self::OverloadedFunctionAsVariable { name } => {
                write!(f, "Cannot use overloaded function {name} as variable")
            }
            Self::IncompatibleTypes { type0, type1 } => {
                write!(f, "Incompatible types: {type0} and {type1}")
            }
            Self::NotAFunction { type_ } => {
                write!(
                    f,
                    "expression does not evaluate to a function (got {type_})"
                )
            }
            Self::NotACompound { type_ } => {
                write!(
                    f,
                    "expression does not evaluate to a compound (got {type_})"
                )
            }
            Self::DynamicFunctionAsVariable { name } => {
                write!(
                    f,
                    "Cannot use unspecialized dynamic function {name} as variable"
                )
            }
            Self::InvalidAutoLocation => {
                write!(f, "the auto type \"$\" can only be placed inside turbofish specializations")
            }
            Self::AutoSpecializationWithoutCall => {
                write!(f, "overloads specialized with auto \"$\" mut be called immediately")
            }
        }
    }
}

pub enum ResolvedTracedCompilationError {
    Syntax(Box<pest::error::Error<Rule>>),
    Compilation(ResolvedCompilationError, usize, String),
}

impl Display for ResolvedTracedCompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Syntax(e) => Display::fmt(e, f),
            Self::Compilation(r, start_line, errant_area) => write!(
                f,
                "{} {{{}| {}}} [{}]",
                r,
                start_line,
                errant_area,
                <&ResolvedCompilationError as Into<&'static str>>::into(r)
            ),
        }
    }
}
