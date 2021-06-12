/*!
`&str` -> `Vec<Token>`
*/

mod tk;

pub use tk::{Token, TokenKind};

use crate::span::ByteSpan;

#[derive(Debug, Clone)]
pub enum LexError {
    //
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    Lexer::lex(src)
}

/// Stateful lexer
struct Lexer<'a> {
    /// Source string slice referenced as bytes
    ///
    /// Because we're only interested in ASCII characters while lexing, we're stroing the source
    /// string as bytes.
    src: &'a [u8],
    /// Span of the source code currently handled
    sp: ByteSpan,
    /// Output tokens
    ///
    /// Token as return value makes the lexing difficult when we want to lex multiple tokens at
    /// once. Instead, we're storing tokens in this field and some lexing methods will mutate it.
    tks: Vec<Token>,
}

#[inline(always)]
fn is_ws(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | b'\t')
}

#[inline(always)]
fn is_digit(c: u8) -> bool {
    !(c < b'0' || b'9' < c)
}

#[inline(always)]
fn is_num_body(c: u8) -> bool {
    is_digit(c) || matches!(c, b'.' | b'E' | b'_')
}

#[inline(always)]
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
            if self.sp.lo >= self.src.len() {
                break;
            }

            self.lex_forward()?;
        }

        Ok(self.tks)
    }

    #[inline(always)]
    fn lex_forward(&mut self) -> Result<(), LexError> {
        if let Some(tk) = self.lex_one_byte() {
            self.tks.push(tk);
            return Ok(());
        }

        if let Some(tk) = self.lex_ws() {
            self.tks.push(tk);
            return Ok(());
        }

        todo!()
    }

    #[inline(always)]
    fn lex_one_byte(&mut self) -> Option<Token> {
        let c = self.src[self.sp.lo];

        self::tk_byte(c).map(|kind| self.consume_span_as(kind))
    }

    #[inline(always)]
    fn lex_ws(&mut self) -> Option<Token> {
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

    /// [0-9]<num>*
    #[inline(always)]
    fn lex_num(&mut self) -> Option<Token> {
        let c = self.src[self.sp.lo];
        if !self::is_digit(c) {
            return None;
        }
        todo!()
    }

    /// [^0-9]<ws>*
    #[inline(always)]
    fn lex_ident(&mut self) -> Option<Token> {
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
