/*!
Tree representation of source code with indices to interned crate data
*/

use crate::{
    ir::intern::{data::*, interner::*},
    utils::arena::{Arena, Idx},
};

#[derive(Debug, Clone)]
pub struct CrateTree {
    pub tk: CrateToken,
    pub procs: Vec<Idx<DefProc>>,
    // pub vars: Vec<Idx<DefVar>>,
    pub mods: Arena<ModuleTree>,
    /// Implicit root module
    pub root: ModuleId,
}

impl CrateTree {
    pub fn new(tk: CrateToken, root: ModuleId) -> Self {
        Self {
            tk,
            procs: Default::default(),
            mods: Default::default(),
            root,
        }
    }

    /// Allocates a module just under the crate root
    pub fn insert_module(&mut self, krate: CrateToken, id: ModuleId) -> Idx<ModuleTree> {
        let tree = ModuleTree::sub_module(krate, id, self.root);
        self.mods.alloc(tree)
    }

    /// Allocates a sub module of a module
    pub fn insert_sub_module(&mut self, id: ModuleId, parent: ModuleId) -> Idx<ModuleTree> {
        let tree = ModuleTree::sub_module(self.tk.clone(), id, parent);
        self.mods.alloc(tree)
    }
}

/// Subtree of modules
#[derive(Debug, Clone)]
pub struct ModuleTree {
    pub krate: CrateToken,
    pub id: ModuleId,
    pub parent: Option<ModuleId>,
    pub children: Vec<ModuleId>,
    pub procs: Vec<Idx<DefProc>>,
}

impl ModuleTree {
    /// Implicit crate root module
    pub fn crate_root(krate: CrateToken, id: ModuleId) -> Self {
        Self {
            krate: krate.clone(),
            id,
            parent: None,
            children: vec![],
            procs: vec![],
        }
    }

    /// Sub module of a parent modue
    pub fn sub_module(krate: CrateToken, id: ModuleId, parent: ModuleId) -> Self {
        Self {
            krate: krate.clone(),
            id,
            parent: Some(parent),
            children: vec![],
            procs: vec![],
        }
    }
}
