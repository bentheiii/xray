pub struct RuntimeLimits {
    pub size_limit: usize,
    pub depth_limit: usize,
    pub recursion_limit: usize,
}

pub struct Runtime {
    pub limits: RuntimeLimits,
    pub size: usize,
}