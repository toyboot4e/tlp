//! Item list

pub mod item;

mod item_source_map;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use std::ops;

use crate::hir_def::{
    db::vfs::VfsFileId,
    ids::{HirItemLoc, Id, Name},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemList {
    pub(crate) file: VfsFileId,
    pub(crate) procs: Arena<item::DefProc>,
    // pub(crate) imports: Vec<Import>,
}

impl ops::Index<Idx<item::DefProc>> for ItemList {
    type Output = item::DefProc;
    fn index(&self, ix: Idx<item::DefProc>) -> &Self::Output {
        &self.procs[ix]
    }
}

impl ItemList {
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

/// Items visible in a scope (declarations and imports)
///
/// Built upon `ItemTree`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemScope {
    // declarations
    procs: FxHashMap<Name, Id<HirItemLoc<item::DefProc>>>,
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Id<HirItemLoc<item::DefProc>>) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Name) -> Option<Id<HirItemLoc<item::DefProc>>> {
        self.procs.get(name).cloned()
    }
}
