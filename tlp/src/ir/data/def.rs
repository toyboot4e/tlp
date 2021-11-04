/*!
HIR item definitions, views to `ItemTree` items
*/

use la_arena::Idx;

use crate::ir::data::decl::{Name, Visibility};

use super::res::ModuleData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId {
    // krate: CrateId,
    // block: Option<BlockId>,
    /// The module's ID in its originating `DefMap`.
    idx: Idx<ModuleData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockLoc {
    // ast_id: AstId<ast::BlockExpr>,
    /// The containing module.
    module: ModuleId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemPath {
    segments: Vec<Name>,
}

/// Unresolved path to a type, often referred to as `Id<TypePath>`
///
/// In RA, it's known as `TypeRef`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePath {
    repr: ItemPath,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcData {
    pub name: Name,
    // TODO: where is the variable name..?
    // pub params: Vec<Id<TypePath>>,
    // pub ret_ty: Id<TypePath>,
    // pub vis: Visibility,
}
