//! Frontend IR

pub mod body;
pub mod db;
pub mod expr;
pub mod item;
pub mod lower;
pub mod pat;
pub mod scope;

use la_arena::{Arena, Idx};

use std::sync::Arc;

use self::{
    db::vfs::VfsFileId,
    scope::{ItemList, ItemScope},
};

/// [`FileData`] container
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

    pub fn sub_file(&self, module: FileDataId) -> &FileData {
        &self.files[module.idx]
    }
}

/// File items
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileData {
    pub(crate) file: VfsFileId,
    pub(crate) parent: Option<FileDataId>,
    pub(crate) children: Vec<FileDataId>,
    pub(crate) scope: Arc<ItemScope>,
}

impl FileData {
    pub fn scope(&self) -> &ItemScope {
        &self.scope
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileDataId {
    // krate: CrateId,
    // block: BlockId,
    pub(crate) idx: Idx<FileData>,
}
