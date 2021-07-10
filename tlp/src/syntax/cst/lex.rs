/*!
Stream of tokens
*/

use thiserror::Error;

use crate::syntax::{
    cst::data::SyntaxKind,
    span::{ByteSpan, TextPos},
};

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
    Eof { at: TextPos },
}

impl LexError {
    pub fn span(&self) -> ByteSpan {
        match self {
            Self::Unreachable { sp } => sp.clone(),
            Self::Eof { at: pos } => ByteSpan::at(*pos),
        }
    }
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
fn is_paren(c: u8) -> bool {
    matches!(c, b'(' | b')')
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
    !(is_ws(c) || is_paren(c) || c == b'"')
}

#[inline(always)]
fn is_ident_start(c: u8) -> bool {
    is_ident_body(c) && !is_num_start(c)
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
    fn advance_if(&mut self, p: impl Fn(u8) -> bool) -> Option<()> {
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
    fn advance_while(&mut self, p: impl Fn(u8) -> bool) {
        while self.sp.hi < self.src.len() {
            let peek = self.src[self.sp.hi];

            if !p(peek) {
                return;
            }

            self.sp.hi += 1;
        }
    }

    #[inline(always)]
    fn advance_until(&mut self, p: impl Fn(u8) -> bool) {
        while self.sp.hi < self.src.len() {
            let peek = self.src[self.sp.hi];
            self.sp.hi += 1;

            if p(peek) {
                return;
            }
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

        if let Some(tk) = self.lex_comment() {
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
        start: impl Fn(u8) -> bool,
        body: impl Fn(u8) -> bool,
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

    /// Any kind of comment (oneline/multiline, normal/docstring)
    #[inline(always)]
    fn lex_comment(&mut self) -> Option<Token> {
        self.advance_if(|b| b == b';')?;
        self.advance_until(|b| b == b'\n');
        Some(self.consume_span_as(SyntaxKind::Comment))
    }

    /// [0-9]<num>*
    #[inline(always)]
    fn lex_num(&mut self) -> Option<Token> {
        self.lex_syntax(self::is_num_start, self::is_num_body, SyntaxKind::Num)
    }

    /// "[^"]*"
    #[inline(always)]
    fn process_str(&mut self) -> Option<()> {
        self.advance_if(|b| b == b'"')?;
        self.push_span_as(SyntaxKind::StrEnclosure);

        self.advance_while(|b| b != b'"');
        // always push the span as content even if it's empty
        self.push_span_as(SyntaxKind::StrContent);

        if self.advance_if(|b| b == b'"').is_some() {
            self.push_span_as(SyntaxKind::StrEnclosure);
        } else {
            self.errs.push(LexError::Eof { at: self.src.len() });
            self.consume_span();
        }

        Some(())
    }

    /// [^<ws>]*
    #[inline(always)]
    fn lex_ident(&mut self) -> Option<Token> {
        self.lex_syntax(self::is_ident_start, self::is_ident_body, SyntaxKind::Ident)
    }
}
