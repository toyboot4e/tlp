/*!
Lowers AST into IR

# The lowering path

It collects crates, modules and items (type/procedure definitions) interning locations.
*/

pub mod def;
pub mod loc;
pub mod tree;

use thiserror::Error;

use crate::{db::intern::*, syntax::ast::data as ast, utils::arena::Idx};

use self::{def::*, loc::*, tree::*};

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

#[derive(Debug, Clone, Error)]
pub enum InternError {
    #[error("Duplicate function definition: {s}")]
    DupFn { access: AbsAccess, s: String },
    #[error("Duplicate module definition: {s}")]
    DupMod { access: AbsAccess, s: String },
}
