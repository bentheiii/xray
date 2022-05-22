use std::sync::Arc;
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Position;
use crate::parser::Rule;
use string_interner::StringInterner;
use crate::xscope::Identifier;
use crate::{XCompilationScopeItem, XCompoundSpec, XType};
use crate::xexpr::XExpr;
use strum::IntoStaticStr;

#[derive(Debug)]
pub struct TracedCompilationError(CompilationError, ((usize, usize), usize), ((usize, usize), usize));

#[derive(Debug, IntoStaticStr)]
pub enum CompilationError {
    VariableTypeMismatch{
        variable_name: Identifier,
        expected_type: Arc<XType>,
        actual_type: Arc<XType>,
    },
    RequiredParamsAfterOptionalParams{
        function_name: Identifier,
        param_name: Identifier,
    },
    DefaultEvaluationError {
        function_name: Identifier,
        param_name: Identifier,
        error: String,  // todo fix when we have proper eval error handling
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
    NotAFunction {type_: Arc<XType>},
    NotACompound {type_: Arc<XType>},
}

impl CompilationError{
    pub fn display_with_interner(&self, interner: &StringInterner)->String{
        match self {
            CompilationError::VariableTypeMismatch{variable_name, expected_type, actual_type} => {
                format!("Variable {} has type {}, but expected {}",
                    interner.resolve(variable_name.clone()).unwrap(),
                    actual_type.display_with_interner(interner),
                    expected_type.display_with_interner(interner)
                )
            }
            CompilationError::RequiredParamsAfterOptionalParams{function_name, param_name} => {
                format!("Required parameter {} after optional parameter in function {}",
                    interner.resolve(param_name.clone()).unwrap(),
                    interner.resolve(function_name.clone()).unwrap()
                )
            }
            CompilationError::DefaultEvaluationError{function_name, param_name, error} => {
                format!("Error evaluating default value for parameter {} in function {}: {}",
                    interner.resolve(param_name.clone()).unwrap(),
                    interner.resolve(function_name.clone()).unwrap(),
                    error
                )
            }
            CompilationError::FunctionOutputTypeMismatch{function_name, expected_type, actual_type} => {
                format!("Function {} has output type {}, but expected {}",
                    interner.resolve(function_name.clone()).unwrap(),
                    actual_type.display_with_interner(interner),
                    expected_type.display_with_interner(interner)
                )
            }
            CompilationError::TypeNotFound{name} => {
                format!("Type {} not found", name)
            }
            CompilationError::GenericParamCountMismatch{type_name, expected_count, actual_count} => {
                format!("Type {} has {} generic parameters, but expected {}",
                    type_name,
                    actual_count,
                    expected_count
                )
            }
            CompilationError::ValueIsNotType{name, item} => {
                format!("{} is not of type (found {:?})",
                    interner.resolve(name.clone()).unwrap(),
                    item,
                )
            }
            CompilationError::PairNotType => {
                format!("Expression cannot be interpreted as a typer")
            }
            CompilationError::NameAlreadyDefined {name, other} => {
                format!("Name {} is already defined as {:?}", interner.resolve(name.clone()).unwrap(), other)
            }
            CompilationError::AmbiguousOverload {name, is_generic, items, param_types} => {
                format!("Overload{} for {} is ambiguous for param types {:?}: {:?}",
                    if *is_generic { " (generic)" } else { "" },
                    interner.resolve(name.clone()).unwrap(),
                    param_types,
                    items
                )
            }
            CompilationError::NoOverload {name,param_types} => {
                format!("No overload for {} found for param types {:?}",
                    interner.resolve(name.clone()).unwrap(),
                    param_types
                )
            }
            CompilationError::VariantConstructorOneArg => {
                format!("Variant constructors must have exactly one argument")
            }
            CompilationError::VariantConstructorTypeArgMismatch {union_name, variant_name, expected_type, actual_type} => {
                format!("Variant {} of union {} has type {}, but expected {}",
                    variant_name,
                    interner.resolve(union_name.clone()).unwrap(),
                    actual_type.display_with_interner(interner),
                    expected_type.display_with_interner(interner)
                )
            }
            CompilationError::StructParamsLengthMismatch {struct_name, expected_count, actual_count} => {
                format!("Struct {} has {} parameters, but expected {}",
                    interner.resolve(struct_name.clone()).unwrap(),
                    actual_count,
                    expected_count
                )
            }
            CompilationError::StructFieldTypeMismatch {struct_name, expected_types, actual_types} => {
                format!("Struct {} has parameters of types [{:?}], but expected [{:?}]",
                    interner.resolve(struct_name.clone()).unwrap(),
                    actual_types.iter().map(|t| t.display_with_interner(interner)).join(", "),
                    expected_types.iter().map(|t| t.display_with_interner(interner)).join(", ")
                )
            }
            CompilationError::NonFunctionSpecialization {name, item} => {
                format!("Cannot specialize non-function {} (found {:?})",
                    interner.resolve(name.clone()).unwrap(),
                    item,
                )
            }
            CompilationError::SpecializedFunctionTypeMismatch {name, idx, expected_type, actual_type} => {
                format!("Specialized argument at index {} of function {} has type {:?}, but expected {:?}",
                    idx,
                    interner.resolve(name.clone()).unwrap(),
                    actual_type,
                    expected_type,
                )
            }
            CompilationError::FunctionNotFound {name} => {
                format!("Function {} not found", interner.resolve(name.clone()).unwrap())
            }
            CompilationError::MemberNotFound {spec, name} => {
                format!("Member {} not found in compound {}", name, interner.resolve(spec.name).unwrap())
            }
            CompilationError::NonCompoundMemberAccess {xtype} => {
                format!("Cannot access member of non-compound type {}", xtype.display_with_interner(interner))
            }
            CompilationError::ValueNotFound {name} => {
                format!("Value {} not found", interner.resolve(name.clone()).unwrap())
            }
            CompilationError::TypeAsVariable {name} => {
                format!("Cannot use type {} as variable", interner.resolve(name.clone()).unwrap())
            }
            CompilationError::GenericFunctionAsVariable {name} => {
                format!("Cannot use generic function {} as variable", interner.resolve(name.clone()).unwrap())
            }
            CompilationError::OverloadedFunctionAsVariable {name} => {
                format!("Cannot use overloaded function {} as variable", interner.resolve(name.clone()).unwrap())
            }
            CompilationError::IncompatibleTypes {type0, type1} => {
                format!("Incompatible types: {} and {}", type0.display_with_interner(interner), type1.display_with_interner(interner))
            }
            CompilationError::NotAFunction {type_} => {
                format!("expression does not evaluate to a function (got {})", type_.display_with_interner(interner))
            }
            CompilationError::NotACompound {type_} => {
                format!("expression does not evaluate to a compound (got {})", type_.display_with_interner(interner))
            }
        }
    }

    pub fn trace(self, input: &Pair<Rule>) -> TracedCompilationError {
        fn pos_to_coors(pos: &Position) -> ((usize, usize), usize) {
            (pos.line_col(), pos.pos())
        }
        let start = input.as_span().start_pos();
        let end = input.as_span().end_pos();
        TracedCompilationError(self, pos_to_coors(&start), pos_to_coors(&end))
    }
}

impl TracedCompilationError {
    pub fn display(&self, interner: &StringInterner, input: &str) -> String {
        let ((start_line, _), start_pos) = self.1;
        let (_, end_pos) = self.2;
        let slc = &input[start_pos..=end_pos];
        format!("{} {{{}| {}}} [{}]", self.0.display_with_interner(interner), start_line, slc, <&CompilationError as Into<&'static str>>::into(&self.0))
    }
}