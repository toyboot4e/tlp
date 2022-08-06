//! Functgion body

// not fully resolved (for example function calls)

pub mod expr;

use la_arena::{Arena, Idx};

use crate::hir_def::path::Pattern;

use self::expr::Expr;

/// Lowered code block
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub root: Block,
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pattern>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Block {
    pub children: Vec<Idx<Expr>>,
}
