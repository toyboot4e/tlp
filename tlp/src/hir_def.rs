//! Intermadiate representation between AST and bytecode

pub mod body;
pub mod db;
pub mod item;
pub mod lower;
pub mod path;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use std::{ops, sync::Arc};

use crate::{hir_def::db::vfs::*, syntax::ast};

use self::{
    db::{
        ids::{Id, Loc},
        vfs::VfsFileId,
    },
    item::{Name, Visibility},
};

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
    pub(crate) vis: Visibility,
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

/// Name-resolved item definitions IDs in a scope (declarations and imports)
///
/// Built upon `ItemTree`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemScope {
    // declarations
    procs: FxHashMap<Name, Id<Loc<item::DefProc>>>,
    // values:
    // delcs
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Id<Loc<item::DefProc>>) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Name) -> Option<Id<Loc<item::DefProc>>> {
        self.procs.get(name).cloned()
    }
}

/// Top-level item declarations in a file
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileItemList {
    pub(crate) file: VfsFileId,
    pub(crate) procs: Arena<item::DefProc>,
    // pub(crate) imports: Vec<Import>,
}

impl ops::Index<Idx<item::DefProc>> for FileItemList {
    type Output = item::DefProc;
    fn index(&self, ix: Idx<item::DefProc>) -> &Self::Output {
        &self.procs[ix]
    }
}

impl FileItemList {
    pub(crate) fn new(file: VfsFileId) -> Self {
        Self {
            file,
            procs: Default::default(),
        }
    }

    pub fn procs(&self) -> &Arena<item::DefProc> {
        &self.procs
    }
}
