use std::cell::RefCell;
use std::fmt::Debug;
use std::io::Write;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct RuntimeLimits {
    pub size_limit: Option<usize>,
    pub depth_limit: Option<usize>,
    pub recursion_limit: Option<usize>,
}

impl RuntimeLimits {
    pub fn to_runtime<W: Write + 'static>(self, output: W) -> RTCell<W> {
        Rc::new(RefCell::new(Runtime {
            limits: self,
            size: 0,
            stdout: output,
        }))
    }
}

pub struct Runtime<W: Write + 'static> {
    pub(crate) limits: RuntimeLimits,
    pub(crate) size: usize, // this will be zero if the runtime has no size limit
    pub stdout: W,
}

pub type RTCell<W> = Rc<RefCell<Runtime<W>>>;
