/*!
File I/O and crate graph
*/

use std::hash;

use crate::{
    ir::intern::interner::{AccessId, ModuleId, ScopeId},
    syntax::cst::data as cst,
};

/// Represents a crate, but doesn't contain anything in it
///
/// TODO: Intern
#[derive(Debug, Clone)]
pub struct CrateToken {
    pub name: String,
}

/// Represents a module, but doesn't contain anything in it
#[derive(Debug, Clone)]
pub struct ModuleToken {
    pub access: AbsAccess,
}

impl ModuleToken {
    pub fn crate_root(krate: CrateToken) -> Self {
        Self {
            access: AbsAccess::crate_root(krate),
        }
    }
}

/// Relative path in toylisp source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelAccess {
    pub name: cst::SyntaxToken,
}

impl hash::Hash for RelAccess {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        // TODO: don't allocate string
        self.name.to_string().hash(state);
    }
}

/// Absolute path to an item
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct AbsAccess {
    // TODO: include crate index
}

impl AbsAccess {
    /// TODO: allow any lexical scope
    pub fn new(_scope: ScopeId, _access: &RelAccess) -> Self {
        Self {}
    }

    pub fn crate_root(_kraet: CrateToken) -> Self {
        Self {}
    }
}

/// Locagion of lexical scope in a crate
#[derive(Debug, Clone)]
pub enum Scope {
    /// Just under the crate
    Crate {},
    /// Just under the module
    Module { module: ModuleId },
    LexScope {
        module: ModuleId,
        access: AccessId,
        pos: usize,
    },
}
