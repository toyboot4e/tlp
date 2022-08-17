//! Frontend IR
//!
//! # Arenas and ownership
//!
//! AST is lowered into [`Arena`] s in item level and expression level:
//!
//! - [`ItemList`] owns arenas of definition syntaxes
//! - [`Body`] owns arenas of expressions and patterns
//!
//! Other data structures refere to the data by [`Idx`] to the arenas. More global [`Idx`] data type
//! is known as locations (e.g. [`ItemLoc`]).
//!
//! # Data flow
//!
//! - For each module, lower AST into [`FileData`], i.e., [`ItemList`]
//! - For each module, collect [`ItemList`] data into [`ItemScope`]
//! - For each declaration, lower the code block into [`Body`]
//!   - For each (nested or root) code block, lower items into [`ItemList`]
//! - Create [`ExprScopeMap`] for [`Body`]
//!   - For each (nested or root) code block, collect [`ScopeData`]
//!
//! [`Arena`]: la_arena::Arena
//! [`Idx`]: la_arena::Idx
//! [`ItemLoc`]: crate::hir_def::db::ids::ItemLoc
//!
//! [`FileData`]: crate::hir_def::FileData
//! [`Body`]: crate::hir_def::body::Body
//!
//! [`ItemScope`]: crate::hir_def::scope::ItemScope
//! [`ExprScope`]: crate::hir_def::scope::ExprScope
//! [`ScopeData`]: crate::hir_def::scope::ScopeData
//! [`ExprScopeMap`]: crate::hir_def::scope::ExprScopeMap

pub mod body;
pub mod db;
pub mod expr;
pub mod item;
pub mod lower;
pub mod pat;
pub mod scope;

use std::sync::Arc;

use la_arena::{Arena, Idx};

use self::{
    db::vfs::VfsFileId,
    scope::{ItemList, ItemScope},
};

/// Per-project [`FileData`] container
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CrateData {
    pub(crate) root: FileDataId,
    /// Sub module files
    pub(crate) files: Arena<FileData>,
    // TODO: collect diagnostics
    // pub(crate) diags: Vec<Diagnostic>,
}

impl CrateData {
    pub fn new(root: FileDataId) -> Self {
        Self {
            root,
            files: Arena::default(),
        }
    }

    pub fn root_file_data_id(&self) -> FileDataId {
        self.root
    }

    pub fn root_file_data(&self) -> &FileData {
        &self.files[self.root.idx]
    }

    pub fn sub_file(&self, module: FileDataId) -> &FileData {
        &self.files[module.idx]
    }
}

/// Pre-file [`ItemScope`] with child/parent relationship
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileData {
    pub(crate) file: VfsFileId,
    pub(crate) parent: Option<FileDataId>,
    pub(crate) children: Vec<FileDataId>,
    /// Items visible from this file (defined or imported)
    pub item_scope: Arc<ItemScope>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileDataId {
    // krate: CrateId,
    // block: BlockId,
    pub(crate) idx: Idx<FileData>,
}

// TODO: add data for macro-expanded `ItemList` (use it for crate/block DefMap)
