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
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    /// TODO: lifetime?
    pub name: rowan::SyntaxText,
    // pub ast: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    Int(i64, Option<BuiltinInt>),
    Uint(u64, Option<BuiltinUint>),
    Float(f64, Option<BuiltinFloat>),
}

impl cmp::Eq for Literal {}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BuiltinInt {
    I8,
    I16,
    I32,
    I64,
}

/// Different unsigned int types.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BuiltinUint {
    U8,
    U16,
    U32,
    U64,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BuiltinFloat {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Char,
    Bool,
    Str,
    Int(BuiltinInt),
    Uint(BuiltinUint),
    Float(BuiltinFloat),
}

impl From<ast::Num> for Literal {
    fn from(x: ast::Num) -> Self {
        let text = x.syntax().text();

        // TODO: parse explicit type name

        if let Ok(x) = text.parse::<u64>() {
            return Self::Uint(x, None);
        }

        if let Ok(x) = text.parse::<i64>() {
            return Self::Int(x, None);
        }

        if let Ok(x) = text.parse::<f64>() {
            return Self::Float(x, None);
        }

        todo!("if not a number");
    }
}
