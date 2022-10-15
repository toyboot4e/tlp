//! Lexer / tokenizer

use base::span::Span;
use thiserror::Error;

use crate::syntax::cst::SyntaxKind;

/// Text span with syntactic kind
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: SyntaxKind,
    // TODO: prefer `&str`?
    pub span: Span,
}

impl Token {
    pub fn slice<'s>(&self, src: &'s str) -> &'s str {
        self.span.slice(src)
    }
}

/// Lexical error type
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum LexError {
    // TODO: while what?
    #[error("Unexpected end of file ({span:?})")]
    UnterminatedString { span: Span },
}

impl LexError {
    /// Returns a detailed error message
    pub fn detailed_message(&self, src: &str) -> String {
        match self {
            LexError::UnterminatedString { .. } => "unterminated string",
        }
        .to_string()
    }

    /// Returns a simplified error message, useful when quoting source text.
    pub fn simple_message(&self, _: &str) -> String {
        match self {
            LexError::UnterminatedString { .. } => "unterminated string",
        }
        .to_string()
    }
}

/// Convers text into a CST. It doesn't fail even if the given text has wrong syntax.
pub fn from_str<'s>(src: &'s str) -> (Vec<Token>, Vec<LexError>) {
    let lex = Lexer {
        src: src.as_bytes(),
        sp: Span::from(0u32, 0u32),
        tks: vec![],
        errs: vec![],
    };
    lex.run()
}

/// Stateful lexer that converts given string into simple [`Token`] s
#[derive(Debug)]
struct Lexer<'s> {
    /// We treat the UTF-8 source string as bytes. This is valid since we're only interested in
    /// ASCII characteres while lexing, and all of them are single byte. Single byte characters in
    /// UTF-8 are all a byte starting with `0`.
    src: &'s [u8],
    sp: Span,
    tks: Vec<Token>,
    errs: Vec<LexError>,
}

/// "Whitespace" = trivia
fn is_ws(c: u8) -> bool {
    matches!(c, b' ' | b'\n' | b'\t')
}

/// Single-character-delimited token
fn is_symbol(c: u8) -> bool {
    matches!(c, b'(' | b')' | b':' | b'.')
}

fn is_quote(c: u8) -> bool {
    matches!(c, b'"' | b'\'')
}

/// [0-9] | -
fn is_num_start(c: u8) -> bool {
    !(c < b'0' || b'9' < c) || c == b'-'
}

/// [0-9] | . | E | _
fn is_num_body(c: u8) -> bool {
    !(c < b'0' || b'9' < c) || matches!(c, b'.' | b'e' | b'E' | b'_' | b'-')
}

fn is_ident_start(c: u8) -> bool {
    is_ident_body(c) && !is_num_start(c)
}

fn is_ident_body(c: u8) -> bool {
    !(is_ws(c) || is_symbol(c) || is_quote(c))
}

/// Lexing utilities
impl<'s> Lexer<'s> {
    fn consume_span(&mut self) -> Span {
        let sp = self.sp.clone();
        self.sp.start = self.sp.end;
        sp
    }

    fn consume_span_as(&mut self, kind: SyntaxKind) -> Token {
        let sp = self.consume_span();
        Token { kind, span: sp }
    }

    /// The predicate returns if we should continue scanning reading a byte
    fn advance_if(&mut self, p: impl Fn(u8) -> bool) -> Option<()> {
        if self.sp.end.into_usize() >= self.src.len() {
            return None;
        }

        let peek = self.src[self.sp.end.into_usize()];

        if p(peek) {
            self.sp.end += 1u32;
            Some(())
        } else {
            None
        }
    }

    /// The predicate returns if we should continue scanning reading a byte
    fn advance_while(&mut self, p: impl Fn(u8) -> bool) {
        while self.sp.end.into_usize() < self.src.len() {
            let peek = self.src[self.sp.end.into_usize()];

            if !p(peek) {
                return;
            }

            self.sp.end += 1u32;
        }
    }

    fn advance_until(&mut self, p: impl Fn(u8) -> bool) {
        while self.sp.end.into_usize() < self.src.len() {
            let peek = self.src[self.sp.end.into_usize()];
            self.sp.end += 1u32;

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
        while self.sp.start.into_usize() < self.src.len() {
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
        let c = self.src[self.sp.end.into_usize()];

        let kind = match c {
            b'(' => SyntaxKind::LParen,
            b')' => SyntaxKind::RParen,
            b':' => SyntaxKind::Colon,
            b'.' => SyntaxKind::Dot,
            b'-' => {
                // peek one character
                if let Some(c2) = self.src.get(self.sp.end.into_usize() + 1usize) {
                    match *c2 {
                        // number
                        _ if self::is_num_body(*c2) => {
                            return None;
                        }
                        // right arrow
                        b'>' => {
                            debug_assert_eq!(self.sp.start, self.sp.end);
                            self.sp.end += 2u32;
                            let tk = self.consume_span_as(SyntaxKind::RightArrow);
                            return Some(tk);
                        }
                        // minus operator
                        _ => {}
                    }
                }
                SyntaxKind::Ident
            }
            _ => return None,
        };

        self.sp.end += 1u32;
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
        while self.sp.end.into_usize() < self.src.len() {
            let b = self.src[self.sp.end.into_usize()];

            // consider method call
            if b == b'.' {
                let peek = self.sp.end + 1u32;
                if peek.into_usize() < self.src.len()
                    && !self::is_num_body(self.src[peek.into_usize()])
                {
                    break;
                }
            }

            if !self::is_num_body(b) {
                break;
            }

            self.sp.end += 1u32;
        }
        Some(self.consume_span_as(SyntaxKind::Num))
    }

    /// "[^"]*"
    fn lex_str(&mut self) -> Option<Token> {
        self.advance_if(|b| b == b'"')?;
        self.advance_while(|b| b != b'"');
        if self.advance_if(|b| b == b'"').is_none() {
            self.errs.push(LexError::UnterminatedString {
                span: Span {
                    start: self.sp.start,
                    end: self.src.len().into(),
                },
            });
            // No early return; allow non-terminated string at EoF
        }

        Some(self.consume_span_as(SyntaxKind::Str))
    }

    /// [^<ws>]*
    fn lex_ident_or_kwd(&mut self) -> Option<Token> {
        let mut tk = lex_ident(self)?;

        // If it's a keyword, override the syntax kind
        let s: &str = unsafe { std::str::from_utf8_unchecked(self.src) };
        let s = tk.slice(s);

        // overwrite keyword's syntax kind
        if let Some(kind) = to_kwd(s) {
            tk.kind = kind;
        }

        return Some(tk);

        /// [^<ws>]*
        fn lex_ident(me: &mut Lexer) -> Option<Token> {
            me.lex_syntax(self::is_ident_start, self::is_ident_body, SyntaxKind::Ident)
        }

        fn to_kwd(kwd: &str) -> Option<SyntaxKind> {
            Some(match kwd {
                // literals are recognized
                "true" => SyntaxKind::True,
                "false" => SyntaxKind::False,
                "nil" => SyntaxKind::Nil,

                // REMARK: keywords other than literals are not lexed
                _ => {
                    return None;
                }
            })
        }
    }
}
