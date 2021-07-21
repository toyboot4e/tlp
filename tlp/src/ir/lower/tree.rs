/*!
Tree representation of source code built aruond interned data
*/

use crate::{
    db::intern::*,
    ir::lower::{def::*, loc::*},
    utils::arena::{Arena, Idx},
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
    pub id: Module,
    pub parent: Option<Module>,
    pub children: Vec<Module>,
    pub procs: Vec<Idx<DefProc>>,
}

impl ModuleTree {
    /// Implicit crate root module
    fn crate_root(krate: CrateLoc, id: Module) -> Self {
        Self {
            krate: krate.clone(),
            id,
            parent: None,
            children: vec![],
            procs: vec![],
        }
    }

    /// Create a sub module of a parent modue
    fn sub_module(krate: CrateLoc, id: Module, parent: Module) -> Self {
        Self {
            krate: krate.clone(),
            id,
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
