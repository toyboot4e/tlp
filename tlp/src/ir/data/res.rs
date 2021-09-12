/*!
Module item definitions after import resolution and macro expansion
*/

use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use crate::ir::{data::decl::Name, db::ids::*};

pub type LocalModuleId = Idx<ModuleData>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrateDefMap {
    root: LocalModuleId,
    /// Module data, including the root
    module: Arena<ModuleData>,
    // diags: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleOrigin {
    Root,
    File,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleData {
    origin: ModuleOrigin,
    // visibility: Visibility,
    parent: Option<LocalModuleId>,
    children: Vec<LocalModuleId>,
    // scope: ScopeData,
}

/// Items visible from a scope
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemScope {
    // upcast?
    procs: FxHashMap<Name, ProcId>,
}
