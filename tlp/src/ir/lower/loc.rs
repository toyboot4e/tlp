/*!
Locations
*/

use std::marker::PhantomData;

use crate::{db::intern::Module, syntax::cst::data as cst};

/// Represents a crate, but doesn't contain anything in it
///
/// TODO: Intern
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrateLoc {
    pub name: String,
}

/// Represents a module, but doesn't contain anything in it
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleLoc {
    pub access: AbsAccess,
}

impl ModuleLoc {
    pub fn crate_root(krate: CrateLoc) -> Self {
        Self {
            access: AbsAccess::crate_root(krate),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleItemLoc<T> {
    module: Module,
    _ty: PhantomData<T>,
}

/// Relative path in toylisp source code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RelAccess {
    pub name: cst::SyntaxToken,
}

/// Absolute path to an item
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct AbsAccess {
    // TODO: include crate index
}

impl AbsAccess {
    // /// TODO: allow any lexical scope
    // pub fn new( _access: &RelAccess) -> Self {
    //     Self {}
    // }

    pub fn crate_root(_kraet: CrateLoc) -> Self {
        Self {}
    }
}
