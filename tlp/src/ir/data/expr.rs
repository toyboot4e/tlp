/*!
HIR expression (unresolved syntax separated from AST, still disugared)
*/

use la_arena::Idx;

use crate::ir::data::decl::Name;

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    String(String),
    Char(char),
    Bool(bool),
    Int(i64, Option<BuiltinInt>),
    Uint(u64, Option<BuiltinUint>),
    Float(u64, Option<BuiltinFloat>),
}

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
