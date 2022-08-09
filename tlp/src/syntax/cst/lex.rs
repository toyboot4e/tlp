//! Lexer / tokenizer

use thiserror::Error;

use crate::syntax::{
    cst::SyntaxKind,
    span::{ByteSpan, TextPos},
};

/// Text span with syntactic kind
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: SyntaxKind,
    // TODO: prefer `&str`?
    pub sp: ByteSpan,
}

impl Token {
    pub fn slice<'s>(&self, src: &'s str) -> &'s str {
        self.sp.slice(src)
    }
}

/// Lexical error type
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum LexError {
    // TODO: use line:column representation on print
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: ByteSpan },
    // TODO: while what?
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
    /// We treat the UTF-8 source string as bytes. This is valid since we're only interested in
    /// ASCII characteres while lexing, and all of them are single byte. Single byte characters in
    /// UTF-8 are all a byte starting with `0`.
    src: &'s [u8],
    sp: ByteSpan,
    tks: Vec<Token>,
    errs: Vec<LexError>,
}

/// Is trivia
fn is_ws(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | b'\t')
}

/// Single-character-delimited token
fn is_symbol(c: u8) -> bool {
    matches!(c, b'(' | b')' | b':' | b'.')
}

/// [0-9] | -
fn is_num_start(c: u8) -> bool {
    !(c < b'0' || b'9' < c) || c == b'-'
}

/// [0-9] | . | E | _
fn is_num_body(c: u8) -> bool {
    !(c < b'0' || b'9' < c) || matches!(c, b'.' | b'e' | b'E' | b'_' | b'-')
}

fn is_ident_body(c: u8) -> bool {
    !(is_ws(c) || is_symbol(c) || c == b'"')
}

fn is_ident_start(c: u8) -> bool {
    is_ident_body(c) && !is_num_start(c) && !is_symbol(c)
}

/// Lexing utilities
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

    /// The predicate returns if we should continue scanning reading a byte
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
    fn advance_while(&mut self, p: impl Fn(u8) -> bool) {
        while self.sp.hi < self.src.len() {
            let peek = self.src[self.sp.hi];

            if !p(peek) {
                return;
            }

            self.sp.hi += 1;
        }
    }

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

macro_rules! apply_syntax {
    ($obj:ident, $syn:tt) => {
        if let Some(tk) = $obj.$syn() {
            $obj.tks.push(tk);
            return;
        }
    };
    ($obj:ident, $($syn:tt),* $(,)?) => {
        $(apply_syntax!($obj, $syn);)*
    };
}

impl<'s> Lexer<'s> {
    pub fn run(mut self) -> (Vec<Token>, Vec<LexError>) {
        while self.sp.lo < self.src.len() {
            self.process_forward();
        }

        (self.tks, self.errs)
    }

    /// Processes one token/node and stores those tokens in `self.tks`
    fn process_forward(&mut self) {
        apply_syntax!(
            self,
            lex_one_byte,
            lex_comment,
            lex_ws,
            lex_num,
            lex_str,
            lex_ident_or_kwd,
        );

        unreachable!("lex error at span {:?}", self.sp);
    }
}

/// Syntax utilities
impl<'s> Lexer<'s> {
    /// Lexes tokens in syntax `Start Body*`
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

/// Grammers (&mut self → Option<Token>)
impl<'s> Lexer<'s> {
    /// Byte tokens: `():.-`
    fn lex_one_byte(&mut self) -> Option<Token> {
        let c = self.src[self.sp.hi];

        let kind = match c {
            b'(' => SyntaxKind::LParen,
            b')' => SyntaxKind::RParen,
            b':' => SyntaxKind::Colon,
            b'.' => SyntaxKind::Dot,
            b'-' => {
                if let Some(c2) = self.src.get(self.sp.hi + 1) {
                    if self::is_num_body(*c2) {
                        return None;
                    }
                }
                SyntaxKind::Ident
            }
            _ => return None,
        };

        self.sp.hi += 1;
        Some(self.consume_span_as(kind))
    }

    /// Trivia
    fn lex_ws(&mut self) -> Option<Token> {
        self.lex_syntax(&self::is_ws, &self::is_ws, SyntaxKind::Ws)
    }

    /// Any kind of comment (oneline/multiline, normal/docstring)
    fn lex_comment(&mut self) -> Option<Token> {
        self.advance_if(|b| b == b';')?;
        self.advance_until(|b| b == b'\n');
        Some(self.consume_span_as(SyntaxKind::Comment))
    }

    /// [0-9]<num>*
    fn lex_num(&mut self) -> Option<Token> {
        self.advance_if(self::is_num_start)?;
        while self.sp.hi < self.src.len() {
            let b = self.src[self.sp.hi];

            // consider method call
            if b == b'.' {
                let peek = self.sp.hi + 1;
                if peek < self.src.len() && !self::is_num_body(self.src[peek]) {
                    break;
                }
            }

            if !self::is_num_body(b) {
                break;
            }

            self.sp.hi += 1;
        }
        Some(self.consume_span_as(SyntaxKind::Num))
    }

    /// "[^"]*"
    fn lex_str(&mut self) -> Option<Token> {
        self.advance_if(|b| b == b'"')?;
        self.advance_while(|b| b != b'"');
        if self.advance_if(|b| b == b'"').is_none() {
            self.errs.push(LexError::Eof { at: self.src.len() });
            // No early return; allow non-terminated string at EoF
        }

        Some(self.consume_span_as(SyntaxKind::Str))
    }

    /// [^<ws>]*
    fn lex_ident_or_kwd(&mut self) -> Option<Token> {
        let mut tk = lex_ident(self)?;

        // If it's a keyword, override the syntax kind
        let s: &str = unsafe { std::str::from_utf8_unchecked(self.src) };
        if let Some(kind) = parse_kwd(s) {
            tk.kind = kind;
        }

        return Some(tk);

        /// [^<ws>]*
        fn lex_ident(me: &mut Lexer) -> Option<Token> {
            me.lex_syntax(self::is_ident_start, self::is_ident_body, SyntaxKind::Ident)
        }

        fn parse_kwd(kwd: &str) -> Option<SyntaxKind> {
            Some(match kwd {
                "true" => SyntaxKind::True,
                "false" => SyntaxKind::False,
                "nil" => SyntaxKind::Nil,
                "let" => SyntaxKind::Let,
                _ => {
                    return None;
                }
            })
        }
    }
}
