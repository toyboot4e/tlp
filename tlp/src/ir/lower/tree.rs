/*!
Tree representation of source code built aruond interned data
*/

use crate::ir::{
    db::ids::*,
    lower::{def::*, loc::*},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrateTree {
    pub krate: Crate,
    pub modules: Vec<Module>,
    /// Implicit root module
    pub root: Module,
}

impl CrateTree {
    pub fn new(tk: Crate, root: Module) -> Self {
        Self {
            krate: tk,
            modules: Vec::new(),
            root,
        }
    }
}

/// Subtree of modules
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleTree {
    pub krate: CrateLoc,
    pub module: Module,
    pub parent: Option<Module>,
    pub children: Vec<Module>,
    /// TODO: Consider non-linear search for duplication check on insertion
    pub procs: Vec<DefProc>,
    // sub_modules: Vec<ModuleTree>,
}

impl ModuleTree {
    /// Implicit crate root module
    pub fn crate_root(krate: CrateLoc, module: Module) -> Self {
        Self {
            krate,
            module,
            parent: None,
            children: vec![],
            procs: vec![],
        }
    }

    /// Create a sub module of a parent modue
    pub fn sub_module(krate: CrateLoc, module: Module, parent: Module) -> Self {
        Self {
            krate,
            module,
            parent: Some(parent),
            children: vec![],
            procs: vec![],
        }
    }

    pub fn insert_sub_module(&self, id: Module) -> Self {
        // let tree = Self::sub_module
        todo!()
    }
}
