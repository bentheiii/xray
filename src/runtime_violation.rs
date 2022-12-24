use std::rc::Rc;

// these are errors at which runtime is aborted
#[derive(Debug, Clone)]
pub enum RuntimeViolation {
    AllocationLimitReached,
    MaximumRecursion,
    MaximumStackDepth,
    MaximumUDCall,
    MaximumSearch,
    OutputFailure(Rc<std::io::Error>),
    Timeout,
    PermissionError(&'static str),
}
