/*!
Lowered code block
*/

use la_arena::{Arena, Idx};

use crate::ir::data::expr::Expr;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub(crate) exprs: Arena<Expr>,
    // TOOD: separate BlockDefMap from CrateDefMap and add block scope stack
}

//
// AST node ID → HIR node ID
