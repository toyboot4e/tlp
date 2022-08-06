//! HIR item definitions; views to `ItemTree` items

use la_arena::Arena;

use crate::hir_def::{
    decl::Name,
    item::{expr::Expr, path::Pattern},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcData {
    pub name: Name,
    // TODO: where is the variable name..?
    // pub params: Vec<Id<TypePath>>,
    // pub ret_ty: Id<TypePath>,
    // pub vis: Visibility,
}

/// Lowered code block
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Body {
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pattern>,
    // TOOD: separate BlockDefMap from CrateDefMap and add block scope stack
}
