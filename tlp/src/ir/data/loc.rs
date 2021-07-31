/*!
Locations

Path to anything is called "access", which is backed by `camino` path buffer type. Locations of
items are newtypes of the access and interned by salsa database.
*/

use std::marker::PhantomData;

use camino::{Utf8Path, Utf8PathBuf};

use crate::{ir::db::ids::Module, syntax::cst::data as cst};

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

/// Path to a crate
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrateLoc {
    /// Different from Rust, each crate has unique name in toylisp
    name: String,
}

impl CrateLoc {
    pub fn new(name: String) -> Option<Self> {
        // TODO: Validate the name
        Some(Self { name })
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    // pub fn join_module(&self, s: &str) -> ModuleLoc {
}

/// Path to a module
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
