/*!
Stream of tokens
*/

use thiserror::Error;

use crate::syntax::{cst::data::SyntaxKind, span::ByteSpan};

/// Span of text with syntactic kind
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub sp: ByteSpan,
}

impl Token {
    pub fn slice<'s>(&self, src: &'s str) -> &'s str {
        self.sp.slice(src)
    }
}

/// Error type accumulated while lexing
#[derive(Debug, Clone, Error)]
pub enum LexError {
    // TODO: use line:column representation
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: ByteSpan },
    #[error("Unexpected end of file")]
    Eof,
}

/// Convers text into a CST. It doesn't fail even if the given text has wrong syntax.
pub fn from_str<'s>(src: &'s str) -> (Vec<Token>, Vec<LexError>) {
    let l = Lexer {
        src: src.as_bytes(),
        sp: Default::default(),
        tks: vec![],
        errs: vec![],
    };
    l.run()
}

/// Stateful lexer that converts given string into simple [`Token`] s
#[derive(Debug)]
struct Lexer<'s> {
    src: &'s [u8],
    sp: ByteSpan,
    tks: Vec<Token>,
    errs: Vec<LexError>,
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
    !(is_ws(c) || matches!(c, b'"'))
}

#[inline(always)]
fn is_ident_start(c: u8) -> bool {
    !(is_ws(c) || matches!(c, b'"') || is_num_start(c))
}

#[inline(always)]
fn tk_byte(c: u8) -> Option<SyntaxKind> {
    let kind = match c {
        b'(' => SyntaxKind::LParen,
        b')' => SyntaxKind::RParen,
        _ => return None,
    };

    Some(kind)
}

impl<'s> Lexer<'s> {
    fn consume_span(&mut self) -> ByteSpan {
        let sp = self.sp.clone();
        self.sp.lo = self.sp.hi;
        sp
    }

    fn consume_span_as(&mut self, kind: SyntaxKind) -> Token {
        let sp = self.consume_span();
        Token { kind, sp }
    }

    fn push_span_as(&mut self, kind: SyntaxKind) {
        let tk = self.consume_span_as(kind);
        self.tks.push(tk);
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

impl<'s> Lexer<'s> {
    pub fn run(mut self) -> (Vec<Token>, Vec<LexError>) {
        while self.sp.lo < self.src.len() {
            self.process_forward();
        }

        (self.tks, self.errs)
    }

    #[inline(always)]
    fn process_forward(&mut self) {
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

        if let Some(()) = self.process_str() {
            return;
        }

        if let Some(mut tk) = self.lex_ident() {
            // override the token kind with reserved keyword tokens
            let s: &str = unsafe { std::str::from_utf8_unchecked(self.src) };
            match tk.sp.slice(s) {
                "true" => {
                    tk.kind = SyntaxKind::True;
                }
                "false" => {
                    tk.kind = SyntaxKind::False;
                }
                "nil" => {
                    tk.kind = SyntaxKind::Nil;
                }
                _ => {}
            }

            self.tks.push(tk);
            return;
        }

        unreachable!("lex error at span {:?}", self.sp);
    }
}

/// Utilities
impl<'s> Lexer<'s> {
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
        kind: SyntaxKind,
    ) -> Option<Token> {
        self.advance_if(start)?;
        self.advance_while(body);
        Some(self.consume_span_as(kind))
    }
}

/// Lexing syntax
impl<'s> Lexer<'s> {
    #[inline(always)]
    fn lex_ws(&mut self) -> Option<Token> {
        self.lex_syntax(&self::is_ws, &self::is_ws, SyntaxKind::Ws)
    }

    /// [0-9]<num>*
    #[inline(always)]
    fn lex_num(&mut self) -> Option<Token> {
        self.lex_syntax(&self::is_num_start, &self::is_num_body, SyntaxKind::Num)
    }

    /// "[^"]*"
    #[inline(always)]
    fn process_str(&mut self) -> Option<()> {
        self.advance_if(&|b| b == b'"')?;
        self.push_span_as(SyntaxKind::StrEnclosure);

        self.advance_while(&|b| b != b'"');
        assert!(self.sp.lo != self.sp.hi);
        self.push_span_as(SyntaxKind::StrContent);
        assert_eq!(self.sp.lo, self.sp.hi);

        if self.advance_if(&|b| b == b'"').is_some() {
            self.push_span_as(SyntaxKind::StrEnclosure);
        } else {
            self.errs.push(LexError::Eof);
            self.consume_span();
        }

        Some(())
    }

    /// [^<ws>]*
    #[inline(always)]
    fn lex_ident(&mut self) -> Option<Token> {
        self.lex_syntax(
            &self::is_ident_start,
            &self::is_ident_body,
            SyntaxKind::Ident,
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax::cst::lex;

    fn force_lex(src: &str) -> Vec<Token> {
        let (tks, errs) = lex::from_str(src);

        if !errs.is_empty() {
            let errs = errs.iter().map(|e| format!("{}", e)).collect::<Vec<_>>();
            panic!("{:#?}", errs);
        }

        tks
    }

    #[test]
    fn one_byte_tokens() {
        let src = "()";

        assert_eq!(
            self::force_lex(src),
            vec![
                Token {
                    kind: SyntaxKind::LParen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: SyntaxKind::RParen,
                    sp: ByteSpan { lo: 1, hi: 2 },
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
                    kind: SyntaxKind::LParen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: SyntaxKind::Ws,
                    sp: ByteSpan { lo: 1, hi: 3 },
                },
                Token {
                    kind: SyntaxKind::RParen,
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
                    kind: SyntaxKind::LParen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: SyntaxKind::Ident,
                    sp: ByteSpan { lo: 1, hi: 2 },
                },
                Token {
                    kind: SyntaxKind::Ws,
                    sp: ByteSpan { lo: 2, hi: 3 },
                },
                Token {
                    kind: SyntaxKind::Num,
                    sp: ByteSpan { lo: 3, hi: 4 },
                },
                Token {
                    kind: SyntaxKind::Ws,
                    sp: ByteSpan { lo: 4, hi: 5 },
                },
                Token {
                    kind: SyntaxKind::Num,
                    sp: ByteSpan { lo: 5, hi: 6 },
                },
                Token {
                    kind: SyntaxKind::RParen,
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
                kind: SyntaxKind::Nil,
                sp: ByteSpan { lo: 0, hi: 3 },
            }],
        );
    }

    #[test]
    fn string() {
        let src = r##"("str!")"##;
        //            0 2 4 67

        assert_eq!(
            self::force_lex(src),
            vec![
                Token {
                    kind: SyntaxKind::LParen,
                    sp: ByteSpan { lo: 0, hi: 1 },
                },
                Token {
                    kind: SyntaxKind::StrEnclosure,
                    sp: ByteSpan { lo: 1, hi: 2 },
                },
                Token {
                    kind: SyntaxKind::StrContent,
                    sp: ByteSpan { lo: 2, hi: 6 },
                },
                Token {
                    kind: SyntaxKind::StrEnclosure,
                    sp: ByteSpan { lo: 6, hi: 7 },
                },
                Token {
                    kind: SyntaxKind::RParen,
                    sp: ByteSpan { lo: 7, hi: 8 },
                },
            ],
        );
    }
}