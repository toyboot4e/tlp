/*!
`&str` -> `Vec<Token>`
*/

use thiserror::Error;

use crate::span::ByteSpan;

#[derive(Debug, Clone)]
pub enum LexError {
    //
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    Lexer::lex(src)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Ws,
    // ----------------------------------------
    // Symbols
    /// `(`
    ParenOpen,
    /// `)`
    ParenClose,
    // ----------------------------------------
    // ?
    Ident,
    // ----------------------------------------
    // literals
    /// `true`
    True,
    /// `false`
    False,
    /// `nil`
    Nil,
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

struct Lexer<'a> {
    src: &'a [u8],
    /// Span of the source code currently handled
    sp: ByteSpan,
    tks: Vec<Token>,
}

#[inline]
fn is_ws(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | b'\t')
}

#[inline]
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

    fn consume_span_as(&mut self, kind: TokenKind) -> Token {
        let sp = self.consume_span();
        Token { sp, kind }
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
        loop {
            // include the first byte to the span
            self.sp.hi += 1;

            // return if it's finished
            if self.sp.hi > self.src.len() {
                break;
            }

            self.advance()?;
        }

        Ok(self.tks)
    }

    fn advance(&mut self) -> Result<(), LexError> {
        if let Some(tk) = self.tk_one_byte() {
            self.tks.push(tk);
            return Ok(());
        }

        if let Some(tk) = self.tk_ws() {
            self.tks.push(tk);
            return Ok(());
        }

        todo!()
    }

    #[inline]
    fn tk_one_byte(&mut self) -> Option<Token> {
        let c = self.src[self.sp.lo];

        self::tk_byte(c).map(|kind| self.consume_span_as(kind))
    }

    #[inline]
    fn tk_ws(&mut self) -> Option<Token> {
        let c = self.src[self.sp.lo];
        if !is_ws(c) {
            return None;
        }

        while self.sp.hi < self.src.len() {
            self.sp.hi += 1;

            let c = &self.src[self.sp.hi];

            if !self::is_ws(*c) {
                break;
            }
        }

        Some(self.consume_span_as(TokenKind::Ws))
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

    #[test]
    fn ws() -> Result<(), LexError> {
        let src = "( \n)";
        let tks = crate::lex::lex(src)?;

        assert_eq!(
            tks,
            vec![
                Token {
                    kind: TokenKind::ParenOpen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: TokenKind::Ws,
                    sp: ByteSpan { lo: 1, hi: 3 },
                },
                Token {
                    kind: TokenKind::ParenClose,
                    sp: ByteSpan { lo: 3, hi: 4 },
                },
            ]
        );

        Ok(())
    }
}
