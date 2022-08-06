//! Intermadiate representation between AST and bytecode

pub mod body;
pub mod db;
pub mod decl;
pub mod lower;
pub mod path;

use std::sync::Arc;

// Module item definitions. Import are resolved into [`ItemScope`] and top-level macros are
// expanded.

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use self::{
    db::{
        ids::{Id, Loc},
        vfs::FileId,
    },
    decl::{Name, Visibility},
};

/// ID of [`ModuleData`]
pub type ModuleId = Idx<ModuleData>;

/// Name-resolved crate data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CrateDefMap {
    pub(crate) root: ModuleId,
    /// Module data, including the root
    pub(crate) modules: Arena<ModuleData>,
    // pub(crate) diags: Vec<Diagnostic>,
}

impl CrateDefMap {
    pub fn new(root: ModuleId) -> Self {
        Self {
            root,
            modules: Arena::default(),
        }
    }

    pub fn root(&self) -> ModuleId {
        self.root
    }

    pub fn module(&self, module: ModuleId) -> &ModuleData {
        &self.modules[module]
    }
}

/// Name-resolved module data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleData {
    pub(crate) file: FileId,
    pub(crate) vis: Visibility,
    pub(crate) parent: Option<ModuleId>,
    pub(crate) children: Vec<ModuleId>,
    pub(crate) scope: Arc<ItemScope>,
}

impl ModuleData {
    pub fn scope(&self) -> &ItemScope {
        &self.scope
    }
}

/// Name-resolved top-level module items
///
/// Built upon `ItemTree`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemScope {
    procs: FxHashMap<Name, Id<Loc<decl::DefProc>>>,
    // values:
    // delcs
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Id<Loc<decl::DefProc>>) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Name) -> Option<Id<Loc<decl::DefProc>>> {
        self.procs.get(name).cloned()
    }
}
