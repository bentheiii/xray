use std::collections::HashMap;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Permission{pub id: &'static str, pub default: bool}

impl Permission {
    pub const fn new(id: &'static str, default: bool) -> Self {
        Self { id, default }
    }

    pub const fn new_default_allowed(id: &'static str) -> Self {
        Self::new(id, true)
    }

    pub const fn new_default_forbidden(id: &'static str) -> Self {
        Self::new(id, false)
    }
}

#[derive(Debug, Default)]
pub struct PermissionSet (HashMap<&'static str, bool>);

impl PermissionSet {
    pub fn get(&self, permission: &Permission) -> bool {
        self.0.get(permission.id).unwrap_or(&permission.default).clone()
    }

    pub fn allow(&mut self, permission: &Permission) {
        self.0.insert(permission.id, true);
    }

    pub fn forbid(&mut self, permission: &Permission) {
        self.0.insert(permission.id, false);
    }
}
