/*!
Definitions, interned data types
*/

use crate::{
    ir::intern::interner::{AccessId, ScopeId},
    syntax::ast::data as ast,
};

/// Function definition
#[derive(Debug, Clone)]
pub struct DefProc {
    pub access: AccessId,
    pub params: Option<ProcParams>,
    pub scope: ScopeId,
    pub ast: ast::DefProc,
}

impl DefProc {
    pub fn new(ast: ast::DefProc, scope: ScopeId, access: AccessId) -> Self {
        Self {
            access,
            params: ast.params().map(|ast| ProcParams::from_ast(ast)),
            scope,
            ast,
        }
    }
}

/// Function parameters
#[derive(Debug, Clone)]
pub struct ProcParams {
    pub ast: ast::Params,
}

impl ProcParams {
    pub fn from_ast(ast: ast::Params) -> Self {
        Self { ast }
    }
}

/// Multiple lexical scopes in a function body
#[derive(Debug, Clone)]
pub struct LexScope {
    depths: Vec<usize>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Call),
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: AccessId,
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
