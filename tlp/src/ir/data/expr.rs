/*!
Lowered expressions

* Desugared
* No AST information (AST is related via map)
*/

use crate::ir::data::def::Name;

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct Call {
    // TODO: resolved form, path?
    pub name: Name,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub struct Arg {
    /// TODO: lifetime?
    pub name: rowan::SyntaxText,
    // pub ast: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    F32(f32),
}
