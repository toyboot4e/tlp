//! HIR patterns in [`Body`]
//!
//! [`Body`]: crate::hir_def::body::Body
//!
//! Everything that introduces a new variable into scope is a pattern ([`Pat::Bind`]).

// use la_arena::Idx;

// use crate::hir_def::{item::Name, FileData};

use crate::hir_def::ids::Name;

// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct ModuleId {
//     // krate: CrateId,
//     // block: Option<BlockId>,
//     /// The module's ID in its originating `DefMap`.
//     idx: Idx<FileData>,
// }
//
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct BlockLoc {
//     // ast_id: AstId<ast::BlockExpr>,
//     /// The containing module.
//     module: ModuleId,
// }

/// Path to an item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemPath {
    segments: Vec<Name>,
}

impl ItemPath {
    pub fn name(name: Name) -> Self {
        Self {
            segments: vec![name],
        }
    }
}

/// HIR pattern type
///
/// It's more semantic compared to AST patterns.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    /// Given invalid syntax, pattern can miss
    Missing,
    /// Introduces a new identifier
    Bind { name: Name },
}
