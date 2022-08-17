//! Item definition body and code blocks

use la_arena::{Arena, Idx};

use crate::hir_def::{expr, pat};

/// Body
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub root_block: Idx<expr::Expr>,
    pub exprs: Arena<expr::Expr>,
    pub pats: Arena<pat::Pat>,
}

impl Body {
    pub fn root_block(&self) -> &expr::Block {
        match &self.exprs[self.root_block] {
            expr::Expr::Block(seq) => seq,
            _ => unreachable!(),
        }
    }
}
