use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct RuntimeLimits {
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
}

impl RuntimeLimits {
    pub fn to_runtime(self) -> RTCell {
        Rc::new(RefCell::new(Runtime {
            limits: self,
            size: 0,
        }))
    }
}

pub struct Runtime {
    pub(crate) limits: RuntimeLimits,
    pub(crate) size: usize, // this will be zero if the runtime has no size limit
}

pub type RTCell = Rc<RefCell<Runtime>>;
