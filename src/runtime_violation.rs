// these are errors at which runtime is aborted
#[derive(Debug)]
pub enum RuntimeViolation {
    AllocationLimitReached,
    MaximumRecursion,
    MaximumStackDepth,
    MaximumUDCall,
    MaximumSearch,
    OutputFailure(std::io::Error),
}
