use crate::compilation_scope::XCompilationScopeItem;
use crate::parser::Rule;
use crate::xexpr::XExpr;
use crate::Identifier;
use crate::{XCompoundSpec, XType};
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Position;
use std::sync::Arc;
use string_interner::StringInterner;
use strum::IntoStaticStr;

#[derive(Debug)]
pub enum TracedCompilationError {
    Syntax(pest::error::Error<Rule>),
    Compilation(
        CompilationError,
        ((usize, usize), usize),
        ((usize, usize), usize),
    ),
}

#[derive(Debug, IntoStaticStr)]
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
        item: XCompilationScopeItem,
    },
    PairNotType,
    NameAlreadyDefined {
        name: Identifier,
        other: XCompilationScopeItem,
    },
    AmbiguousOverload {
        name: Identifier,
        is_generic: bool,
        items: Vec<XExpr>,
        param_types: Vec<Arc<XType>>,
    },
    NoOverload {
        name: Identifier,
        param_types: Vec<Arc<XType>>,
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
        item: XCompilationScopeItem,
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
    DynamicFunctionAsVariable {
        name: Identifier,
    },
}

impl CompilationError {
    pub fn display_with_interner(&self, interner: &StringInterner) -> String {
        match self {
            CompilationError::VariableTypeMismatch {
                variable_name,
                expected_type,
                actual_type,
            } => {
                format!(
                    "Variable {} has type {}, but expected {}",
                    interner.resolve(variable_name.clone()).unwrap(),
                    actual_type.display_with_interner(interner),
                    expected_type.display_with_interner(interner)
                )
            }
            CompilationError::RequiredParamsAfterOptionalParams {
                function_name,
                param_name,
            } => {
                format!(
                    "Required parameter {} after optional parameter in function {}",
                    interner.resolve(param_name.clone()).unwrap(),
                    function_name.map_or("<lambda>", |function_name| interner
                        .resolve(function_name.clone())
                        .unwrap())
                )
            }
            CompilationError::DefaultEvaluationError {
                function_name,
                param_name,
                error,
            } => {
                format!(
                    "Error evaluating default value for parameter {} in function {}: {}",
                    interner.resolve(param_name.clone()).unwrap(),
                    function_name.map_or("<lambda>", |s| interner.resolve(s.clone()).unwrap()),
                    error
                )
            }
            CompilationError::FunctionOutputTypeMismatch {
                function_name,
                expected_type,
                actual_type,
            } => {
                format!(
                    "Function {} has output type {}, but expected {}",
                    interner.resolve(function_name.clone()).unwrap(),
                    actual_type.display_with_interner(interner),
                    expected_type.display_with_interner(interner)
                )
            }
            CompilationError::TypeNotFound { name } => {
                format!("Type {} not found", name)
            }
            CompilationError::GenericParamCountMismatch {
                type_name,
                expected_count,
                actual_count,
            } => {
                format!(
                    "Type {} has {} generic parameters, but expected {}",
                    type_name, actual_count, expected_count
                )
            }
            CompilationError::ValueIsNotType { name, item } => {
                format!(
                    "{} is not of type (found {:?})",
                    interner.resolve(name.clone()).unwrap(),
                    item,
                )
            }
            CompilationError::PairNotType => {
                format!("Expression cannot be interpreted as a typer")
            }
            CompilationError::NameAlreadyDefined { name, other } => {
                format!(
                    "Name {} is already defined as {:?}",
                    interner.resolve(name.clone()).unwrap(),
                    other
                )
            }
            CompilationError::AmbiguousOverload {
                name,
                is_generic,
                items,
                param_types,
            } => {
                format!(
                    "Overload{} for {} is ambiguous for param types {:?}: {:?}",
                    if *is_generic { " (generic)" } else { "" },
                    interner.resolve(name.clone()).unwrap(),
                    param_types,
                    items
                )
            }
            CompilationError::NoOverload {
                name,
                param_types,
                dynamic_failures,
            } => {
                format!(
                    "No overload for {} found for param types [{}]{}",
                    interner.resolve(name.clone()).unwrap(),
                    param_types
                        .iter()
                        .map(|t| t.display_with_interner(interner))
                        .join(", "),
                    if dynamic_failures.is_empty() {
                        "".to_string()
                    } else {
                        " dynamic failures: ".to_owned() + &dynamic_failures.join(", ")
                    },
                )
            }
            CompilationError::VariantConstructorOneArg => {
                format!("Variant constructors must have exactly one argument")
            }
            CompilationError::VariantConstructorTypeArgMismatch {
                union_name,
                variant_name,
                expected_type,
                actual_type,
            } => {
                format!(
                    "Variant {} of union {} has type {}, but expected {}",
                    variant_name,
                    interner.resolve(union_name.clone()).unwrap(),
                    actual_type.display_with_interner(interner),
                    expected_type.display_with_interner(interner)
                )
            }
            CompilationError::StructParamsLengthMismatch {
                struct_name,
                expected_count,
                actual_count,
            } => {
                format!(
                    "Struct {} has {} parameters, but expected {}",
                    interner.resolve(struct_name.clone()).unwrap(),
                    actual_count,
                    expected_count
                )
            }
            CompilationError::StructFieldTypeMismatch {
                struct_name,
                expected_types,
                actual_types,
            } => {
                format!(
                    "Struct {} has parameters of types [{:?}], but expected [{:?}]",
                    interner.resolve(struct_name.clone()).unwrap(),
                    actual_types
                        .iter()
                        .map(|t| t.display_with_interner(interner))
                        .join(", "),
                    expected_types
                        .iter()
                        .map(|t| t.display_with_interner(interner))
                        .join(", ")
                )
            }
            CompilationError::NonFunctionSpecialization { name, item } => {
                format!(
                    "Cannot specialize non-function {} (found {:?})",
                    interner.resolve(name.clone()).unwrap(),
                    item,
                )
            }
            CompilationError::SpecializedFunctionTypeMismatch {
                name,
                idx,
                expected_type,
                actual_type,
            } => {
                format!("Specialized argument at index {} of function {} has type {:?}, but expected {:?}",
                        idx,
                        interner.resolve(name.clone()).unwrap(),
                        actual_type,
                        expected_type,
                )
            }
            CompilationError::FunctionNotFound { name } => {
                format!(
                    "Function {} not found",
                    interner.resolve(name.clone()).unwrap()
                )
            }
            CompilationError::MemberNotFound { spec, name } => {
                format!(
                    "Member {} not found in compound {}",
                    name,
                    interner.resolve(spec.name).unwrap()
                )
            }
            CompilationError::NonCompoundMemberAccess { xtype } => {
                format!(
                    "Cannot access member of non-compound type {}",
                    xtype.display_with_interner(interner)
                )
            }
            CompilationError::NonItemTupleAccess { member } => {
                format!(
                    "Member access to tuple must be of the for \"item<positive number>\", got {:?}",
                    member
                )
            }
            CompilationError::TupleIndexOutOfBounds {
                tuple_type,
                index,
                max: max_index,
            } => {
                format!(
                    "Tuple index {} out of bounds for tuple {} of size {}",
                    index,
                    tuple_type.display_with_interner(interner),
                    max_index
                )
            }
            CompilationError::ValueNotFound { name } => {
                format!(
                    "Value {} not found",
                    interner.resolve(name.clone()).unwrap()
                )
            }
            CompilationError::TypeAsVariable { name } => {
                format!(
                    "Cannot use type {} as variable",
                    interner.resolve(name.clone()).unwrap()
                )
            }
            CompilationError::GenericFunctionAsVariable { name } => {
                format!(
                    "Cannot use generic function {} as variable",
                    interner.resolve(name.clone()).unwrap()
                )
            }
            CompilationError::OverloadedFunctionAsVariable { name } => {
                format!(
                    "Cannot use overloaded function {} as variable",
                    interner.resolve(name.clone()).unwrap()
                )
            }
            CompilationError::IncompatibleTypes { type0, type1 } => {
                format!(
                    "Incompatible types: {} and {}",
                    type0.display_with_interner(interner),
                    type1.display_with_interner(interner)
                )
            }
            CompilationError::NotAFunction { type_ } => {
                format!(
                    "expression does not evaluate to a function (got {})",
                    type_.display_with_interner(interner)
                )
            }
            CompilationError::NotACompound { type_ } => {
                format!(
                    "expression does not evaluate to a compound (got {})",
                    type_.display_with_interner(interner)
                )
            }
            CompilationError::DynamicFunctionAsVariable { name } => {
                format!(
                    "Cannot use unspecialized dynamic function {} as variable",
                    interner.resolve(name.clone()).unwrap()
                )
            }
        }
    }

    pub fn trace(self, input: &Pair<Rule>) -> TracedCompilationError {
        fn pos_to_coors(pos: &Position) -> ((usize, usize), usize) {
            (pos.line_col(), pos.pos())
        }
        let start = input.as_span().start_pos();
        let end = input.as_span().end_pos();
        TracedCompilationError::Compilation(self, pos_to_coors(&start), pos_to_coors(&end))
    }
}

impl TracedCompilationError {
    pub fn display(&self, interner: &StringInterner, input: &str) -> String {
        match self {
            Self::Syntax(error) => format!("{}", error),
            Self::Compilation(error, ((start_line, _), start_pos), (_, end_pos)) => {
                let slc = &input[*start_pos..*end_pos];
                format!(
                    "{} {{{}| {}}} [{}]",
                    error.display_with_interner(interner),
                    start_line,
                    slc,
                    <&CompilationError as Into<&'static str>>::into(&error)
                )
            }
        }
    }
}
