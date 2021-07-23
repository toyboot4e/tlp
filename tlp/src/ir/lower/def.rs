/*!
Definitions, interned data types

TODO: Use `AstId` indirection and do not dependent on AST nodes directly
*/

use crate::{db::ids::Access, syntax::ast::data as ast};

/// Function definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefProc {
    /// TODO: Replace access with ItemLoc<Self>
    pub access: Access,
    pub params: Option<ProcParams>,
    // pub name: crate::syntax::grammar::IdentString,
    ast: ast::DefProc,
}

impl DefProc {
    pub fn new(ast: ast::DefProc, access: Access) -> Self {
        Self {
            access,
            params: ast.params().map(|ast| ProcParams::from_ast(ast)),
            ast,
        }
    }
}

/// Function parameters
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcParams {
    pub ast: ast::Params,
}

impl ProcParams {
    pub fn from_ast(ast: ast::Params) -> Self {
        Self { ast }
    }
}

/// Code block
pub struct Block {
    // pub scope: LexScope,
    pub ast: ast::Block,
}

// /// Recursive lex scope
// #[derive(Debug, Clone)]
// pub struct LexScope {
//     pub exprs: Vec<Expr>,
//     pub depth: usize,
// }

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Access,
    pub args: Vec<Arg>,
    pub ast: ast::Call,
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
