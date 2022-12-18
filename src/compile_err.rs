use crate::parser::Rule;
use crate::xexpr::XExpr;

use crate::Identifier;
use crate::{XCompoundSpec, XType};
use derivative::Derivative;
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Position;
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::sync::Arc;

use crate::root_compilation_scope::Interner;
use strum::IntoStaticStr;

#[derive(Copy, Clone, Debug)]
pub enum CompilationItemCategory {
    Type,
    Overload,
    Value,
}

impl Display for CompilationItemCategory {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Overload => "overload",
                Self::Type => "type",
                Self::Value => "value",
            }
        )
    }
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub struct TracedCompilationError(
    CompilationError,
    ((usize, usize), usize),
    ((usize, usize), usize),
);

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
pub enum CompilationError {
    VariableTypeMismatch {
        variable_name: Identifier,
        expected_type: Arc<XType>,
        actual_type: Arc<XType>,
    },
    RequiredParamsAfterOptionalParams {
        function_name: Option<Identifier>,
        param_name: Identifier,
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
    PairNotType,
    AmbiguousOverload {
        name: Identifier,
        is_generic: bool,
        items: usize,
        param_types: Option<Vec<Arc<XType>>>,
    },
    NoOverload {
        name: Identifier,
        param_types: Option<Vec<Arc<XType>>>,
        dynamic_failures: Vec<(&'static str, String)>,
    },
    VariantConstructorOneArg,
    VariantConstructorTypeArgMismatch {
        union_name: Identifier,
        variant_name: Identifier,
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
    MemberNotFound {
        spec: Arc<XCompoundSpec>,
        name: Identifier,
    },
    NonCompoundMemberAccess {
        xtype: Arc<XType>,
    },
    NonUnionVariantAccess {
        xtype: Arc<XType>,
    },
    NonVariantMemberAccess {
        xtype: Arc<XType>,
    },
    NonItemTupleAccess {
        member: Identifier,
    },
    TupleIndexOutOfBounds {
        tuple_type: Arc<XType>,
        index: usize,
        max: usize,
    },
    ValueNotFound {
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
    BadEscapeSequence {
        sequence: String,
    },
    SpecializationOfType {
        name: Identifier,
        type_: Arc<XType>,
    },
    SpecializationOfVariable {
        name: Identifier,
    },
    TypeAsVariable {
        type_: Arc<XType>,
    },
    IllegalShadowing {
        name: Identifier,
        current_category: CompilationItemCategory,
        new_category: CompilationItemCategory,
    },
}

trait Resolve {
    type Output;
    fn resolve(&self, interner: &Interner) -> Self::Output;
}

impl Resolve for Arc<XType> {
    type Output = ResolvedType;
    fn resolve(&self, interner: &Interner) -> Self::Output {
        ResolvedType(self.to_string_with_interner(interner))
    }
}

impl Resolve for Identifier {
    type Output = String;
    fn resolve(&self, interner: &Interner) -> Self::Output {
        interner.resolve(*self).unwrap().to_string()
    }
}

impl Resolve for XCompoundSpec {
    type Output = String;
    fn resolve(&self, interner: &Interner) -> Self::Output {
        interner.resolve(self.name).unwrap().to_string()
    }
}

impl<T: Resolve> Resolve for Arc<T> {
    type Output = T::Output;
    fn resolve(&self, interner: &Interner) -> Self::Output {
        self.as_ref().resolve(interner)
    }
}

impl<T: Resolve + Clone> Resolve for Option<T> {
    type Output = Option<T::Output>;
    fn resolve(&self, interner: &Interner) -> Self::Output {
        self.clone().map(|i| i.resolve(interner))
    }
}

impl<T0: Resolve, T1: Resolve> Resolve for (T0, T1) {
    type Output = (T0::Output, T1::Output);
    fn resolve(&self, interner: &Interner) -> Self::Output {
        (self.0.resolve(interner), self.1.resolve(interner))
    }
}

impl<T: Resolve> Resolve for Vec<T> {
    type Output = Vec<T::Output>;
    fn resolve(&self, interner: &Interner) -> Self::Output {
        self.iter().map(|i| i.resolve(interner)).collect()
    }
}

impl<W: Write + 'static> Resolve for Vec<XExpr<W>> {
    type Output = Self;
    fn resolve(&self, _interner: &Interner) -> Self::Output {
        self.clone()
    }
}

macro_rules! trivial_resolve {
    ($type_: ty) => {
        impl Resolve for $type_ {
            type Output = Self;
            fn resolve(&self, _interner: &Interner) -> Self::Output {
                self.clone()
            }
        }
    };
}

trivial_resolve!(String);
trivial_resolve!(&'static str);
trivial_resolve!(bool);
trivial_resolve!(usize);
trivial_resolve!(CompilationItemCategory);

macro_rules! resolve_variants {
    ($self: ident, $interner: expr, $($variant:ident {$($part:ident),*  $(,)?}),+ $(,)?) => {{
        match $self{
            $(
            Self::$variant{$($part),*}=>ResolvedCompilationError::$variant{$($part: $part.resolve($interner)),*}
            ),+
        }
    }}
}

impl Resolve for CompilationError {
    type Output = ResolvedCompilationError;
    fn resolve(&self, interner: &Interner) -> Self::Output {
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
            PairNotType {},
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
            MemberNotFound { spec, name },
            NonCompoundMemberAccess { xtype },
            NonUnionVariantAccess { xtype },
            NonVariantMemberAccess { xtype },
            NonItemTupleAccess { member },
            TupleIndexOutOfBounds {
                tuple_type,
                index,
                max,
            },
            ValueNotFound { name },
            OverloadedFunctionAsVariable { name },
            IncompatibleTypes { type0, type1 },
            NotAFunction { type_ },
            NotACompound { type_ },
            DynamicFunctionAsVariable { name },
            InvalidAutoLocation {},
            AutoSpecializationWithoutCall {},
            BadEscapeSequence { sequence },
            SpecializationOfType { name, type_ },
            SpecializationOfVariable { name },
            TypeAsVariable { type_ },
            IllegalShadowing {
                name,
                current_category,
                new_category,
            },
        )
    }
}

impl CompilationError {
    pub(crate) fn trace(self, input: &Pair<Rule>) -> TracedCompilationError {
        fn pos_to_coors(pos: &Position) -> ((usize, usize), usize) {
            (pos.line_col(), pos.pos())
        }
        let start = input.as_span().start_pos();
        let end = input.as_span().end_pos();
        TracedCompilationError(self, pos_to_coors(&start), pos_to_coors(&end))
    }
}

impl TracedCompilationError {
    pub(crate) fn resolve_with_input(
        self,
        interner: &Interner,
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

pub struct ResolvedType(String);

impl Display for ResolvedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
    PairNotType,
    AmbiguousOverload {
        name: String,
        is_generic: bool,
        items: usize,
        param_types: Option<Vec<ResolvedType>>,
    },
    NoOverload {
        name: String,
        param_types: Option<Vec<ResolvedType>>,
        dynamic_failures: Vec<(&'static str, String)>,
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
    MemberNotFound {
        spec: String,
        name: String,
    },
    NonCompoundMemberAccess {
        xtype: ResolvedType,
    },
    NonUnionVariantAccess {
        xtype: ResolvedType,
    },
    NonVariantMemberAccess {
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
    BadEscapeSequence {
        sequence: String,
    },
    SpecializationOfType {
        name: String,
        type_: ResolvedType,
    },
    SpecializationOfVariable {
        name: String,
    },
    TypeAsVariable {
        type_: ResolvedType,
    },
    IllegalShadowing {
        name: String,
        current_category: CompilationItemCategory,
        new_category: CompilationItemCategory,
    },
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
            Self::PairNotType => {
                write!(f, "Expression cannot be interpreted as a type",)
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
                        " dynamic failures: ".to_owned()
                            + &dynamic_failures
                                .iter()
                                .map(|(a, b)| format!("{a}: {b}"))
                                .join(", ")
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
            Self::MemberNotFound { spec, name } => {
                write!(f, "Member {name} not found in compound {spec}")
            }
            Self::NonCompoundMemberAccess { xtype } => {
                write!(f, "Cannot access member of non-compound type {xtype}")
            }
            Self::NonUnionVariantAccess { xtype } => {
                write!(
                    f,
                    "variant member access syntax (!:/?:) can only be used on unions (got {xtype})"
                )
            }
            Self::NonVariantMemberAccess { xtype } => {
                write!(
                    f,
                    "access to union {xtype} variants must be with the \"?:\" or \"!:\" accessors"
                )
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
                write!(
                    f,
                    "the auto type \"$\" can only be placed inside turbofish specializations"
                )
            }
            Self::AutoSpecializationWithoutCall => {
                write!(
                    f,
                    "overloads specialized with auto \"$\" mut be called immediately"
                )
            }
            Self::BadEscapeSequence { sequence } => {
                write!(f, "bad escape sequence: {sequence}")
            }
            Self::SpecializationOfType { type_, .. } => {
                write!(f, "cannot specialize type {type_}")
            }
            Self::SpecializationOfVariable { name } => {
                write!(f, "cannot specialize variable {name}")
            }
            Self::TypeAsVariable { type_ } => {
                write!(f, "cannot use type {type_} as a variable")
            }
            Self::IllegalShadowing {
                name,
                current_category,
                new_category,
            } => {
                write!(
                    f,
                    "cannot shadow {current_category} {name} with a {new_category}"
                )
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
                errant_area.trim(),
                <&ResolvedCompilationError as Into<&'static str>>::into(r)
            ),
        }
    }
}

impl Debug for ResolvedTracedCompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
