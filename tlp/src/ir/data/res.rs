/*!
Module item definitions after import resolution and macro expansion
*/

use std::sync::Arc;

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use crate::ir::{
    data::decl::{self, Name, Visibility},
    db::input::FileId,
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
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemScope {
    // TODO: upcast to types (ModuleDefId)
    procs: FxHashMap<Name, Idx<decl::DefProc>>,
    // values:
    // delcs
}

impl ItemScope {
    // TODO: upcast to types (ModuleDefId)
    pub(crate) fn declare_proc(&mut self, name: Name, proc: Idx<decl::DefProc>) {
        self.procs.insert(name, proc);
    }
}
