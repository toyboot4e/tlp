//! Lowerd representation of module item definitions (insights)
//!
//! Macros are expanded and imports are resolved.

use crate::hir_def::decl::Name;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcData {
    pub name: Name,
    // pub params: Vec<Id<TypePath>>,
}
