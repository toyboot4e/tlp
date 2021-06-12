/*!
Abstract syntax tree, spans in a hierarchy
*/

use crate::{
    lex::{Token, TokenKind},
    span::ByteSpan,
};

#[derive(Debug, Clone, Default)]
pub struct Ast {
    pub(super) ss: Vec<S>,
}

/// S-expression
#[derive(Debug, Clone, PartialEq)]
pub enum S {
    Literal(Lit),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lit {
    pub kind: LitKind,
    pub sp: ByteSpan,
}

impl Lit {
    /// Creates [`Lit`] WITHOUT ensuring it's valid literal
    pub fn from_tk(tk: Token) -> Option<Self> {
        Some(Self {
            kind: LitKind::from_tk_kind(tk.kind)?,
            sp: tk.sp,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum LitKind {
    Nil,
    Bool,
    Str,
    Num,
}

impl LitKind {
    /// Creates [`LitKind`] WITHOUT ensuring it's valid literal
    pub fn from_tk_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Ws => None,
            TokenKind::ParenOpen => None,
            TokenKind::ParenClose => None,
            TokenKind::True => Some(LitKind::Bool),
            TokenKind::False => Some(LitKind::Bool),
            TokenKind::Nil => Some(LitKind::Nil),
            TokenKind::Num => Some(LitKind::Num),
            TokenKind::StrEnclosure => None,
            TokenKind::StrContent => Some(LitKind::Str),
            TokenKind::Ident => None,
        }
    }
}
