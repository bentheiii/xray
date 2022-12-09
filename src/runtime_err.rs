// these are errors at which runtime is aborted
#[derive(Debug)]
pub enum RuntimeError{
    AllocationLimitReached,
    MaximumRecursion,
    MaximumStackDepth,
    OutputFailure(std::io::Error),
}