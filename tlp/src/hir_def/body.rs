//! Item definition body and code blocks lowered from AST

use la_arena::{Arena, Idx};

use crate::hir_def::{
    expr::{self, Expr},
    pat,
};

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

/// Expr variant getters
impl Body {
    pub fn get_path(&self, path: Idx<expr::Expr>) -> &expr::Path {
        match &self.exprs[path] {
            Expr::Path(path) => path,
            _ => unreachable!(),
        }
    }
}
