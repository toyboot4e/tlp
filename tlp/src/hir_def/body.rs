//! Body of tem definition (procedure or constant)

// not fully resolved (for example function calls)

pub mod expr;

use la_arena::{Arena, Idx};

use crate::hir_def::path::ItemPath;

use self::expr::Expr;

/// Body of tem definition (procedure or constant)
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub root: Block,
    pub exprs: Arena<Expr>,
    pub paths: Arena<ItemPath>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Block {
    pub children: Vec<Idx<Expr>>,
}
