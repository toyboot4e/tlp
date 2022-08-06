//! Functgion body

// not fully resolved (for example function calls)

pub mod expr;

use la_arena::Arena;

use crate::hir_def::{decl::Name, path::Pattern};

use self::expr::Expr;

/// Lowered code block
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pattern>,
}
