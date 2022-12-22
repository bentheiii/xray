use crate::permissions::Permission;
lazy_static! {
    pub static ref SLEEP: Permission = Permission("sleep");
    pub static ref PRINT_DEBUG: Permission = Permission("print_debug");
    pub static ref PRINT: Permission = Permission("print");
    pub static ref RANDOM: Permission = Permission("random");
}
