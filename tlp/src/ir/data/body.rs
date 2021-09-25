/*!
Lowered code block
*/

use la_arena::{Arena, Idx};

use crate::ir::data::expr::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    exprs: Arena<Expr>,
}

//
// AST node ID → HIR node ID
