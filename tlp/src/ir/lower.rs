/*!
Lowers AST into an item tree

# The lowering pass

* The output of the pass is the input to the name resolution pass
* Locations are interned with salsa database (for smaller use of memory)
*/

use thiserror::Error;

use crate::ir::{
    data::{loc::*, tree::*},
    db::ids::*,
};

#[derive(Debug, Clone)]
pub struct CrateData {
    pub tk: Crate,
    pub tree: CrateTree,
}

impl CrateData {
    pub fn new(krate: Crate, root: Module) -> Self {
        CrateData {
            tk: krate,
            tree: CrateTree::new(krate, root),
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq, Eq, Hash)]
pub enum LowerError {
    #[error("Duplicate procedure definition: {s}")]
    DupProc { access: AbsAccess, s: String },
    #[error("Duplicate module definition: {s}")]
    DupMod { access: AbsAccess, s: String },
}
