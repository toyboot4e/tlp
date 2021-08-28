/*!
Tree representation of source code built aruond interned data
*/

use crate::ir::data::def::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemTree {
    pub(crate) procs: Vec<DefProc>,
    // pub(crate) data: Vec<Defata>,
}

impl ItemTree {
    pub fn new() -> Self {
        Self { procs: Vec::new() }
    }
}
