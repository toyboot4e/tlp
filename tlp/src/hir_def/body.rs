//! Body of tem definition (procedure or constant)

// not fully resolved (for example function calls)

use la_arena::{Arena, Idx};

use crate::hir_def::{expr::Expr, pat::Pat};

/// Body
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub root: Block,
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pat>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Block {
    pub children: Vec<Idx<Expr>>,
}
