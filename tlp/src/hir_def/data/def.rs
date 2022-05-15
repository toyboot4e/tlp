//! HIR item definitions; views to `ItemTree` items

use crate::hir_def::data::decl::Name;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcData {
    pub name: Name,
    // TODO: where is the variable name..?
    // pub params: Vec<Id<TypePath>>,
    // pub ret_ty: Id<TypePath>,
    // pub vis: Visibility,
}
