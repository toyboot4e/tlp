/*!
Common data structures among IRs
*/

pub mod def;
pub mod expr;
pub mod scope;

use crate::ir::{data::def::*, db::input::*};

/// IDs of top-level items in a module
///
/// `DeclTree` is known as `ItemTree` in RA.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclTree {
    pub(crate) file: FileId,
    pub(crate) procs: Vec<DefProc>,
    // pub(crate) data: Vec<DefData>,
}

impl DeclTree {
    pub fn new(file: FileId) -> Self {
        Self {
            file,
            procs: Vec::new(),
        }
    }

    pub fn procs(&self) -> &[DefProc] {
        &self.procs
    }
}
