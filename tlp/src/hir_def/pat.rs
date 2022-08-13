//! Patterns

use la_arena::Idx;

use crate::hir_def::{item::Name, FileData};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId {
    // krate: CrateId,
    // block: Option<BlockId>,
    /// The module's ID in its originating `DefMap`.
    idx: Idx<FileData>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockLoc {
    // ast_id: AstId<ast::BlockExpr>,
    /// The containing module.
    module: ModuleId,
}

/// Path to an item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemPath {
    segments: Vec<Name>,
}

/// Pattern
pub type Pat = ItemPath;
