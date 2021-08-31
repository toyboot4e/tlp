/*!
Tree representation of source code built aruond interned data
*/

use crate::ir::{data::def::*, db::input::*};

/// IDs of top-level items in a module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemTree {
    pub(crate) file: FileId,
    pub(crate) procs: Vec<DefProc>,
    // pub(crate) data: Vec<DefData>,
}

impl ItemTree {
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
