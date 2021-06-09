/*!
`&str` -> `Vec<Token>`
*/

use thiserror::Error;

use crate::span::ByteSpan;

#[derive(Debug, Clone, Error)]
pub enum LexError {
    #[error("given empty input")]
    GivenEmptyInput,
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    Lexer::lex(src)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
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

struct Lexer<'a> {
    src: &'a [u8],
    /// Span of the source code currently handled
    sp: ByteSpan,
    tks: Vec<Token>,
}

fn is_ws(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | b'\t')
}

fn tk_byte(c: u8) -> Option<TokenKind> {
    let kind = match c {
        b'(' => TokenKind::ParenOpen,
        b')' => TokenKind::ParenClose,
        b'"' => TokenKind::StrEnclosure,
        _ => return None,
    };

    Some(kind)
}

impl<'a> Lexer<'a> {
    fn consume_span(&mut self) -> ByteSpan {
        let sp = self.sp.clone();
        self.sp.lo = self.sp.hi;
        sp
    }
}

impl<'a> Lexer<'a> {
    pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
        let mut me = Lexer {
            src: src.as_bytes(),
            sp: Default::default(),
            tks: vec![],
        };

        me.lex_impl()
    }

    fn lex_impl(mut self) -> Result<Vec<Token>, LexError> {
        if self.src.is_empty() {
            return Err(LexError::GivenEmptyInput);
        }

        while self.sp.lo < self.src.len() {
            self.advance()?;
        }

        Ok(self.tks)
    }

    fn advance(&mut self) -> Result<(), LexError> {
        // one-char
        self.sp.hi += 1;
        let c = self.src[self.sp.lo];

        if let Some(tk) = self.tk_one_byte(c) {
            self.tks.push(tk);
            return Ok(());
        }

        todo!()
    }

    fn tk_one_byte(&mut self, c: u8) -> Option<Token> {
        self::tk_byte(c).map(|kind| Token {
            sp: self.consume_span(),
            kind,
        })
    }

    fn tk_ws(&mut self) {
        while self.sp.hi < self.src.len() {
            self.sp.hi += 1;

            let c = &self.src[self.sp.hi];

            if self::is_ws(*c) {
                continue;
            } else {
                return;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn one_byte_tokens() -> Result<(), LexError> {
        let src = "(\")";
        let tks = crate::lex::lex(src)?;

        assert_eq!(
            tks,
            vec![
                Token {
                    kind: TokenKind::ParenOpen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: TokenKind::StrEnclosure,
                    sp: ByteSpan { lo: 1, hi: 2 },
                },
                Token {
                    kind: TokenKind::ParenClose,
                    sp: ByteSpan { lo: 2, hi: 3 },
                },
            ]
        );

        Ok(())
    }
}
