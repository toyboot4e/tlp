/*!
Module item definitions. Import are resolved into [`ItemScope`] and top-level macros are expanded
*/

use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use crate::hir_def::{
    data::decl::{self, Name, Visibility},
    db::{
        ids::{AnyDefId, DefId, Id, Loc},
        vfs::FileId,
    },
};

pub type ModuleId = Idx<ModuleData>;

/// Result of name resolution
///
/// - name-declaration map
/// - item scopes
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

/// `ItemTree` with imports resolved
///
/// Every ID in `ItemScope` is upcasted to [`DefId`].
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
