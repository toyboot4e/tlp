/*!
`&str` -> `Vec<Token>`
*/

use thiserror::Error;

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
    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        self.span.slice(src)
    }
}

#[derive(Debug, Clone, Error)]
pub enum TokenizeError {
    #[error("given empty input")]
    GivenEmptyInput,
}

pub struct Tokenizer<'a> {
    src: &'a [u8],
    /// Span of the source code currently handled
    sp: ByteSpan,
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(src: &str) -> Result<Vec<Token>, TokenizeError> {
        let mut me = Tokenizer {
            src: src.as_bytes(),
            sp: Default::default(),
        };

        me.tokenize_impl()
    }

    fn tokenize_impl(&mut self) -> Result<Vec<Token>, TokenizeError> {
        if self.src.is_empty() {
            return Err(TokenizeError::GivenEmptyInput);
        }

        todo!()
    }
}
