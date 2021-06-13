//! Token

use crate::span::ByteSpan;

/// Syntactic kind of Toy Lisp code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ws,
    // ----------------------------------------
    // symbols
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    // ----------------------------------------
    // keywords
    /// `true`
    True,
    /// `false`
    False,
    /// `nil`
    Nil,
    // ----------------------------------------
    // ?
    Ident,
    // ----------------------------------------
    // literals
    Num,
    /// `"`
    StrEnclosure,
    StrContent,
}

/// Span of source text with syntactic kind
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub sp: ByteSpan,
    pub kind: TokenKind,
}

impl Token {
    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        self.sp.slice(src)
    }
}
