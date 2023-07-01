use crate::permissions::Permission;

pub const NOW: Permission = Permission::new_default_allowed("now");
pub const PRINT: Permission = Permission::new_default_allowed("print");
pub const PRINT_DEBUG: Permission = Permission::new_default_allowed("print_debug");
pub const RANDOM: Permission = Permission::new_default_allowed("random");
pub const REGEX: Permission = Permission::new_default_forbidden("regex");
pub const SLEEP: Permission = Permission::new_default_forbidden("sleep");
