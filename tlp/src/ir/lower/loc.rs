/*!
Locations

Path to anything is called "access", which is backed by `camino` path buffer type. Locations of
items are newtypes of the access and interned by salsa database.
*/

use std::marker::PhantomData;

use camino::{Utf8Path, Utf8PathBuf};

use crate::{db::intern::Module, syntax::cst::data as cst};

/// Relative path in toylisp source code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RelAccess {
    pub name: cst::SyntaxToken,
}

/// Absolute path to an item
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct AbsAccess {
    path: Utf8PathBuf,
}

impl AbsAccess {
    // /// TODO: allow any lexical scope
    pub fn new(path: Utf8PathBuf) -> Self {
        Self { path }
    }

    pub fn from_str(s: &str) -> Self {
        Self {
            path: Utf8PathBuf::from(s),
        }
    }

    pub fn as_path(&self) -> &Utf8Path {
        self.path.as_path()
    }

    pub fn to_path_buf(&self) -> Utf8PathBuf {
        self.path.to_path_buf()
    }
}

/// [`AbsAccess`] that only contains identifiers and `:`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamespaceAccess {
    access: AbsAccess,
}

impl NamespaceAccess {
    pub fn new(access: AbsAccess) -> Option<Self> {
        // TODO: Validate the path
        Some(Self { access })
    }

    pub fn from_str(s: &str) -> Option<Self> {
        Self::new(AbsAccess::from_str(s))
    }

    pub fn as_path(&self) -> &Utf8Path {
        self.access.as_path()
    }

    pub fn to_path_buf(&self) -> Utf8PathBuf {
        self.access.to_path_buf()
    }
}

// newtypes of absolute access

/// Represents a crate, but doesn't contain anything in it
///
/// TODO: Intern
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrateLoc {
    access: NamespaceAccess,
}

impl CrateLoc {
    pub fn new(access: NamespaceAccess) -> Option<Self> {
        // TODO: Validate the path
        Some(Self { access })
    }

    pub fn access(&self) -> &NamespaceAccess {
        &self.access
    }
}

/// Represents a module, but doesn't contain anything in it
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleLoc {
    access: NamespaceAccess,
    krate: CrateLoc,
}

impl ModuleLoc {
    pub fn new(access: NamespaceAccess, krate: CrateLoc) -> Self {
        Self { access, krate }
    }

    pub fn access(&self) -> &NamespaceAccess {
        &self.access
    }

    pub fn krate(&self) -> &CrateLoc {
        &self.krate
    }
}

/// Internd location of an item of type T
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleItemLoc<T> {
    module: Module,
    _ty: PhantomData<T>,
}
