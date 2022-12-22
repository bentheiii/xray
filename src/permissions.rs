use std::collections::{HashMap};
use std::ops::Index;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Permission(pub &'static str);

#[derive(Debug)]
pub struct PermissionSet{
    default: bool,
    specific: HashMap<Permission, bool>
}

impl Default for PermissionSet{
    fn default() -> Self {
        Self{default: true, specific: Default::default()}
    }
}

impl Index<&Permission> for PermissionSet{
    type Output = bool;

    fn index(&self, index: &Permission) -> &Self::Output {
        self.specific.get(&index).unwrap_or(&self.default)
    }
}

impl PermissionSet{
    pub fn allow(&mut self, permission: &Permission){
        self.specific.insert(*permission, true);
    }

    pub fn forbid(&mut self, permission: &Permission){
        self.specific.insert(*permission, false);
    }
}