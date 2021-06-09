/*!
`&str` -> `Vec<Token>`
*/

use crate::span::ByteSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Ws,
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    /// `"`
    StrEnclosure,
    StrContent,
    /// `true`
    True,
    /// `false`
    False,
    /// `nil`
    Nil,
}

pub struct Token {
    pub span: ByteSpan,
}

impl Token {
    pub fn slice(&self, src: &str) -> &str {
        self.span.slice(src)
    }
}
