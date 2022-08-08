/*!
HIR expression (unresolved syntax separated from AST, still disugared)
*/

use std::cmp;

use la_arena::Idx;

use crate::{
    hir_def::item::Name,
    syntax::ast::{self, AstToken},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Seq(Seq),
    Call(Call),
    Literal(Literal),
}

/// Sequence of expressions (code block)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Seq {
    pub(crate) exprs: Vec<Idx<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Call {
    // TODO: resolved form, path?
    pub name: Name,
    pub args: Vec<Idx<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    Int(i32),
    Float(EqF32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct EqF32(pub f32);

impl cmp::Eq for EqF32 {}

impl From<ast::Num> for Literal {
    fn from(x: ast::Num) -> Self {
        let text = x.syntax().text();

        if let Ok(x) = text.parse::<i32>() {
            return Self::Int(x);
        }

        if let Ok(x) = text.parse::<f32>() {
            return Self::Float(EqF32(x));
        }

        todo!("if not a number");
    }
}
