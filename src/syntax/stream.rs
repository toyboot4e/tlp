/*!
Stream of tokens
*/

use thiserror::Error;

use crate::span::ByteSpan;

/// Syntactic kind for a unit word of ToyLisp source code
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
    // literals
    Num,
    /// `"`
    StrEnclosure,
    StrContent,
    // ----------------------------------------
    // keywords
    /// `true`
    True,
    /// `false`
    False,
    /// `nil`
    Nil,
    // ?
    Ident,
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

#[derive(Debug, Clone, Error)]
pub enum LexError {
    // TODO: use line:column representation
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: ByteSpan },
}

/// Creates `Vec<Token>` from `&str`
pub fn from_str<'a, 'e>(src: &'a str, errs: &'e mut Vec<LexError>) -> Lexer<'a, 'e> {
    Lexer {
        src: src.as_bytes(),
        sp: Default::default(),
        tks: vec![],
        errs,
    }
}

/// Stateful lexer that converts given string into simple [`Token`] s
pub struct Lexer<'a, 'e> {
    src: &'a [u8],
    sp: ByteSpan,
    tks: Vec<Token>,
    errs: &'e mut Vec<LexError>,
}

#[inline(always)]
fn is_ws(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | b'\t')
}

#[inline(always)]
fn is_num_start(c: u8) -> bool {
    !(c < b'0' || b'9' < c)
}

#[inline(always)]
fn is_num_body(c: u8) -> bool {
    is_num_start(c) || matches!(c, b'.' | b'E' | b'_')
}

#[inline(always)]
fn is_ident_body(c: u8) -> bool {
    !is_ws(c)
}

#[inline(always)]
fn is_ident_start(c: u8) -> bool {
    !(is_ws(c) || is_num_start(c))
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

impl<'a, 'e> Lexer<'a, 'e> {
    fn consume_span(&mut self) -> ByteSpan {
        let sp = self.sp.clone();
        self.sp.lo = self.sp.hi;
        sp
    }

    fn consume_span_as(&mut self, kind: TokenKind) -> Token {
        let sp = self.consume_span();
        Token { sp, kind }
    }

    /// The predicate returns if we should continue scanning reading a byte
    #[inline(always)]
    fn advance_if(&mut self, p: &impl Fn(u8) -> bool) -> Option<()> {
        if self.sp.hi >= self.src.len() {
            return None;
        }

        let peek = self.src[self.sp.hi];

        if p(peek) {
            self.sp.hi += 1;
            Some(())
        } else {
            None
        }
    }

    /// The predicate returns if we should continue scanning reading a byte
    #[inline(always)]
    fn advance_while(&mut self, p: &impl Fn(u8) -> bool) {
        loop {
            if self.sp.hi >= self.src.len() {
                return;
            }

            let peek = self.src[self.sp.hi];

            if !p(peek) {
                return;
            }

            self.sp.hi += 1;
        }
    }
}

impl<'a, 'e> Iterator for Lexer<'a, 'e> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if !self.tks.is_empty() {
                return self.tks.pop();
            }

            if self.sp.lo >= self.src.len() {
                break None;
            } else {
                self.lex_forward();
            }
        }
    }
}

impl<'a, 'e> Lexer<'a, 'e> {
    pub fn run(mut self) -> Vec<Token> {
        self.collect::<Vec<_>>()
    }

    #[inline(always)]
    fn lex_forward(&mut self) {
        if let Some(tk) = self.lex_one_byte() {
            self.tks.push(tk);
            return;
        }

        if let Some(tk) = self.lex_ws() {
            self.tks.push(tk);
            return;
        }

        if let Some(tk) = self.lex_num() {
            self.tks.push(tk);
            return;
        }

        if let Some(mut tk) = self.lex_ident() {
            // override the token kind with reserved keyword tokens
            let s: &str = unsafe { std::str::from_utf8_unchecked(self.src) };
            match tk.sp.slice(s) {
                "true" => {
                    tk.kind = TokenKind::True;
                }
                "false" => {
                    tk.kind = TokenKind::False;
                }
                "nil" => {
                    tk.kind = TokenKind::Nil;
                }
                _ => {}
            }

            self.tks.push(tk);
            return;
        }

        unreachable!("lex error at span {:?}", self.sp);
    }
}

/// Lexing utilities
impl<'a, 'e> Lexer<'a, 'e> {
    #[inline(always)]
    fn lex_one_byte(&mut self) -> Option<Token> {
        let c = self.src[self.sp.hi];

        if let Some(kind) = self::tk_byte(c) {
            self.sp.hi += 1;
            Some(self.consume_span_as(kind))
        } else {
            None
        }
    }

    fn lex_syntax(
        &mut self,
        start: &impl Fn(u8) -> bool,
        body: &impl Fn(u8) -> bool,
        kind: TokenKind,
    ) -> Option<Token> {
        self.advance_if(start)?;
        self.advance_while(body);
        Some(self.consume_span_as(kind))
    }
}

/// Lexing syntax
impl<'a, 'e> Lexer<'a, 'e> {
    #[inline(always)]
    fn lex_ws(&mut self) -> Option<Token> {
        self.lex_syntax(&self::is_ws, &self::is_ws, TokenKind::Ws)
    }

    /// [0-9]<num>*
    #[inline(always)]
    fn lex_num(&mut self) -> Option<Token> {
        self.lex_syntax(&self::is_num_start, &self::is_num_body, TokenKind::Num)
    }

    /// [^<ws>]*
    #[inline(always)]
    fn lex_ident(&mut self) -> Option<Token> {
        self.lex_syntax(
            &self::is_ident_start,
            &self::is_ident_body,
            TokenKind::Ident,
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn force_lex(src: &str) -> Vec<Token> {
        let mut errs = vec![];
        let tks = from_str(src, &mut errs).run();

        if !errs.is_empty() {
            panic!("{:#?}", errs);
        }

        tks
    }

    #[test]
    fn one_byte_tokens() {
        let src = "(\")";

        assert_eq!(
            self::force_lex(src),
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
    }

    #[test]
    fn ws() {
        let src = "( \n)";

        assert_eq!(
            self::force_lex(src),
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
    }

    #[test]
    fn num() {
        let src = "(* 1 3)";
        //         0 2 4 6

        assert_eq!(
            self::force_lex(src),
            vec![
                Token {
                    kind: TokenKind::ParenOpen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: TokenKind::Ident,
                    sp: ByteSpan { lo: 1, hi: 2 },
                },
                Token {
                    kind: TokenKind::Ws,
                    sp: ByteSpan { lo: 2, hi: 3 },
                },
                Token {
                    kind: TokenKind::Num,
                    sp: ByteSpan { lo: 3, hi: 4 },
                },
                Token {
                    kind: TokenKind::Ws,
                    sp: ByteSpan { lo: 4, hi: 5 },
                },
                Token {
                    kind: TokenKind::Num,
                    sp: ByteSpan { lo: 5, hi: 6 },
                },
                Token {
                    kind: TokenKind::ParenClose,
                    sp: ByteSpan { lo: 6, hi: 7 },
                },
            ]
        );
    }

    #[test]
    fn nil() {
        let src = "nil";

        assert_eq!(
            self::force_lex(src),
            vec![Token {
                kind: TokenKind::Nil,
                sp: ByteSpan { lo: 0, hi: 3 },
            }],
        );
    }
}
