//! Frontend IR
//!
//! # Arenas and ownership
//!
//! AST is lowered into [`Arena`] s in item level and expression level:
//!
//! - [`ItemList`] owns arenas of definition syntaxes
//! - [`Body`] owns arenas of expressions and patterns
//!
//! Other data structures refere to the data by local [`Idx`] to the arenas. More global [`Idx`]
//! data type is known as locations (e.g. [`HirItemLoc`]).
//!
//! # Data flow
//!
//! - For each file, create [`FileData`]
//!   - Lower item ASTs into HIR [`ItemList`]
//!   - Collect [`ItemList`] data indices into [`ItemScope`]
//! - For each declaration, lower the AST body into HIR [`Body`]
//!   - Create [`ExprScopeMap`] for [`Body`]
//!   - For each (nested or root) code block, collect items into [`ItemList`]
//!
//! [`Arena`]: la_arena::Arena
//! [`Idx`]: la_arena::Idx
//! [`HirItemLoc`]: crate::hir_def::ids::HirItemLoc
//!
//! [`FileData`]: crate::hir_def::FileData
//! [`Body`]: crate::hir_def::body::Body
//!
//! [`ItemScope`]: crate::hir_def::item_list::ItemScope
//! [`ExprScope`]: crate::hir_def::scope::ExprScope
//! [`ScopeData`]: crate::hir_def::body::expr_scope::ScopeData
//! [`ExprScopeMap`]: crate::hir_def::body::expr_scope::ExprScopeMap
//! [`ItemList`]: crate::hir_def::item_list::ItemList

pub mod body;
pub mod db;
pub mod ids;
pub mod item_list;
pub mod lower;
pub mod resolver;

use std::sync::Arc;

use la_arena::Arena;

use self::{db::vfs::VfsFileId, ids::FileDataLoc};

/// Per-project [`FileData`] container
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CrateData {
    pub(crate) root_file_idx: FileDataLoc,
    /// Sub module files
    pub(crate) files: Arena<FileData>,
    // TODO: collect diagnostics
    // pub(crate) diags: Vec<Diagnostic>,
}

impl CrateData {
    pub fn root_file_data_idx(&self) -> FileDataLoc {
        self.root_file_idx
    }

    pub fn root_file_data(&self) -> &FileData {
        &self.files[self.root_file_idx.idx]
    }

    pub fn sub_file(&self, module: FileDataLoc) -> &FileData {
        &self.files[module.idx]
    }
}

/// Pre-file [`ItemScope`](item_list::ItemScope) with child/parent relationship
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileData {
    pub(crate) file: VfsFileId,
    pub(crate) parent: Option<FileDataLoc>,
    pub(crate) children: Vec<FileDataLoc>,
    /// Items visible from this file (defined or imported)
    pub item_scope: Arc<item_list::ItemScope>,
}

// TODO: add data for macro-expanded `ItemList` (use it for crate/block DefMap)
