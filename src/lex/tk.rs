//! Token

use crate::span::ByteSpan;

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
