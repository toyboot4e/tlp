/*!
Lowered code block
*/

use la_arena::Arena;

use crate::hir_def::data::expr::Expr;

/// HIR procedure body type
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub exprs: Arena<Expr>,
    // TOOD: separate BlockDefMap from CrateDefMap and add block scope stack
}

//
// AST node ID → HIR node ID
