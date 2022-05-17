use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
pub struct RuntimeLimits {
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
}

impl RuntimeLimits {
    pub fn to_runtime(&self) -> RTCell {
        Rc::new(RefCell::new(Runtime {
            limits: self.clone(),
            size: 0,
        }))
    }
}

pub struct Runtime {
    pub limits: RuntimeLimits,
    pub size: usize,  // this will be zero if the runtime has no size limit
}

pub type RTCell = Rc<RefCell<Runtime>>;