/*!
Interners of IR data
*/

use std::collections::{HashMap, HashSet};

use crate::{
    ir::intern::{def::*, krate::*},
    utils::arena::{Arena, Idx},
};

pub type AccessId = Idx<AbsAccess>;
pub type ScopeId = Idx<Scope>;
pub type ModuleId = Idx<ModuleToken>;

#[derive(Debug, Clone, Default)]
pub struct AccessIntern {
    arena: Arena<AbsAccess>,
    log: HashMap<AbsAccess, Idx<AbsAccess>>,
}

impl AccessIntern {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            log: HashMap::new(),
        }
    }

    pub fn get(&self, access: &AbsAccess) -> Option<&AccessId> {
        self.log.get(access)
    }

    pub fn intern(&mut self, access: AbsAccess) -> AccessId {
        match self.log.get(&access) {
            Some(id) => id.clone(),
            None => self.arena.alloc(access),
        }
    }
}

/// Arena without duplicates
#[derive(Debug, Clone)]
pub struct Intern<T> {
    arena: Arena<T>,
    log: HashSet<AbsAccess>,
}

impl<T> Default for Intern<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Intern<T> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            log: HashSet::new(),
        }
    }

    pub fn contains(&self, key: &AbsAccess) -> bool {
        self.log.contains(key)
    }

    pub fn intern(&mut self, data: T, access: AbsAccess) -> Option<Idx<T>> {
        if self.log.contains(&access) {
            None
        } else {
            let idx = self.arena.alloc(data);
            self.log.insert(access);

            Some(idx)
        }
    }
}

/// Interned, untyped crate data
#[derive(Debug, Clone)]
pub struct CrateIntern {
    pub tk: CrateToken,
    /// Modules, including implicit crate root module
    pub modules: Arena<ModuleToken>,
    pub access: AccessIntern,
    pub scopes: Arena<Scope>,
    pub procs: Intern<DefProc>,
}

impl CrateIntern {
    pub fn new(tk: CrateToken, root: ModuleToken) -> Self {
        Self {
            tk,
            modules: Arena::new(),
            access: AccessIntern::new(),
            scopes: Arena::new(),
            procs: Intern::new(),
        }
    }

    pub(crate) fn alloc_crate_root_module(&mut self, krate: CrateToken) -> Idx<ModuleToken> {
        self.modules.alloc(ModuleToken::crate_root(krate))
    }

    // pub fn intern_proc
}
