/*!
Syntax tree: untyped, homogeneous token tree
*/

use std::fmt;

use thiserror::Error;

use crate::{
    span::ByteSpan,
    syntax::{
        self,
        lex::{FlatLexError, Token, TokenKind},
    },
};

/// File string tokenized as S-expressions
#[derive(Debug, Clone, Default)]
pub struct FileLex<'a> {
    /// Source as text
    pub src: &'a str,
    /// Source as tokens
    pub tks: Vec<Token>,
    /// Souce as S-expressions just under the file
    pub sxs: Vec<Sx>,
}

/// Span of [`Token`] s in range `(lo, hi]` referred to as `tsp`
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct TokenSpan {
    pub lo: usize,
    pub hi: usize,
}

impl TokenSpan {
    pub fn slice<'t>(&self, tks: &'t [Token]) -> &'t [Token] {
        &tks[self.lo..self.hi]
    }
}

/// Generic data that represents span of tokens
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<Body> {
    pub tsp: TokenSpan,
    pub body: Body,
}

macro_rules! enum_from {
    ($into:ident, $($ty:ident),*) => {
        $(
            impl From<$ty> for $into {
                fn from(x: $ty) -> Self {
                    Self::$ty(x)
                }
            }
        )*
    };
}

/// S-expression = [`List`] | [`Atom`]
///
/// # Syntax
///
/// * [`Atom`] = [`Symbol`] | [`Lit`]
///     * Symbol = ValSymbol | FnSymbol | Keyword
#[derive(Debug, Clone, PartialEq)]
pub enum Sx {
    Atom(Atom),
    List(List),
}

// S-expression = List | Atom
enum_from!(Sx, List, Atom);

impl Sx {
    /// Token span
    pub fn tsp(&self) -> TokenSpan {
        match self {
            Sx::Atom(a) => a.tsp(),
            Sx::List(l) => l.tsp,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Lit(Lit),
    Symbol(Symbol),
}

// Atom = Lit | Symbol
enum_from!(Atom, Lit, Symbol);

impl Atom {
    /// Token span
    pub fn tsp(&self) -> TokenSpan {
        match self {
            Atom::Lit(l) => l.tsp,
            Atom::Symbol(s) => s.tsp,
        }
    }
}

pub type Lit = Spanned<LitBody>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LitBody {
    Nil,
    Bool,
    Str {
        beg: TokenSpan,
        content: TokenSpan,
        end: TokenSpan,
    },
    Num,
}

impl LitBody {
    pub fn one_tk(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::True => Some(Self::Bool),
            TokenKind::False => Some(Self::Bool),
            TokenKind::Nil => Some(Self::Nil),
            TokenKind::Num => Some(Self::Num),
            _ => None,
        }
    }
}

/// Function call
pub type List = Spanned<ListBody>;

#[derive(Debug, Clone, PartialEq)]
pub struct ListBody {
    pub operands: Vec<Sx>,
}

pub type Symbol = Spanned<SymbolBody>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SymbolBody {
    Ident,
}

// --------------------------------------------------------------------------------
// lexing procedure

pub type Result<T, E = SpannedHieLexError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
pub enum HieLexError {
    // TODO: use line:column representation
    #[error("It doesn't make any sense: {sp:?}")]
    Unreachable { sp: ByteSpan },
    #[error("{src}")]
    LexError {
        #[from]
        src: LexError,
    },
    #[error("Unexpected EoF while parsing")]
    Eof,
    #[error("Expected {expected}, found {found}")]
    Unexpected { expected: String, found: String },
}

#[derive(Debug, Clone, Error)]
pub struct SpannedHieLexError {
    /// Token span required for recovery (maybe someday)
    pub tsp: TokenSpan,
    /// The actual error representation
    pub err: HieLexError,
}

impl std::fmt::Display for SpannedHieLexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.err.fmt(f)
    }
}

/// Creates [`FileLex`] from `&str`
pub fn from_str<'a>(src: &'a str) -> Result<FileLex<'a>> {
    // TODO: fix span
    let (tks, errs) = syntax::stream::from_str(src);

    // TODO: create lossless tree anyways
    if !errs.is_empty() {
        return Err(SpannedHieLexError {
            tsp: TokenSpan::default(),
            err: errs[0].clone().into(),
        });
    }

    self::from_tks(src, tks)
}

/// Creates [`FileLex`] from `&str` and `&[Token]`
pub fn from_tks<'s>(src: &'s str, tks: Vec<Token>) -> Result<FileLex<'s>> {
    let sxs = LexContext::lex(src, &tks)?;
    Ok(FileLex { src, tks, sxs })
}

/// Referred to as `lcx`
#[derive(Debug, Clone)]
struct LexContext<'s, 't>
where
    's: 't,
{
    src: &'s str,
    tks: &'t [Token],
}

/// Referred to as `lcx`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct LexState {
    tsp: TokenSpan,
}

impl<'s, 't> LexContext<'s, 't>
where
    's: 't,
{
    pub fn lex(src: &'s str, tks: &'t [Token]) -> Result<Vec<Sx>> {
        let lcx = Self { src, tks };
        lcx.lex_impl()
    }

    fn lex_impl(self) -> Result<Vec<Sx>> {
        let mut sxs = vec![];

        let mut state = LexState::default();
        while state.tsp.lo < self.tks.len() {
            let sx = state.maybe_sx(&self)?;
            state = LexState::at(sx.tsp().hi);
            sxs.push(sx);
        }

        Ok(sxs)
    }
}

impl LexState {
    /// State parsing from the point
    fn at(hi: usize) -> Self {
        Self {
            tsp: TokenSpan { lo: hi, hi },
        }
    }
}

/// Mutations
impl LexState {
    /// Advances the high end of the span
    #[inline(always)]
    fn skip_ws(&mut self, lcx: &LexContext) -> Option<()> {
        self.skip_tk_kind(lcx, TokenKind::Ws)
    }

    /// Advances the high end of the span when a peeked token matches to given kind
    ///
    /// FIXME: EoF safety
    #[inline(always)]
    fn skip_tk_kind(&mut self, lcx: &LexContext, kind: TokenKind) -> Option<()> {
        if lcx.tks[self.tsp.hi].kind == kind {
            self.tsp.hi += 1;
            Some(())
        } else {
            None
        }
    }

    /// Advances both the high end and the low end of the span
    #[inline(always)]
    fn ignore_ws(&mut self, lcx: &LexContext) -> Option<()> {
        self.ignore_tk_kind(lcx, TokenKind::Ws)
    }

    /// Advances both the high end and the low end of the span when a peeked token matches to given
    /// kind
    ///
    /// FIXME: EoF safety
    #[inline(always)]
    fn ignore_tk_kind(&mut self, lcx: &LexContext, kind: TokenKind) -> Option<()> {
        if lcx.tks[self.tsp.hi].kind == kind {
            self.tsp.hi += 1;
            self.tsp.lo = self.tsp.hi;
            Some(())
        } else {
            None
        }
    }

    #[inline(always)]
    fn peek<'t>(&mut self, lcx: &'t LexContext) -> Option<&'t Token> {
        if self.tsp.hi < lcx.tks.len() {
            Some(&lcx.tks[self.tsp.hi])
        } else {
            None
        }
    }

    #[inline(always)]
    fn try_peek<'t>(&mut self, lcx: &'t LexContext) -> Result<&'t Token> {
        self.peek(lcx).ok_or_else(|| SpannedHieLexError {
            tsp: self.tsp,
            err: HieLexError::Eof,
        })
    }

    #[inline(always)]
    fn advance<'t>(&mut self, lcx: &'t LexContext) -> Option<&'t Token> {
        if self.tsp.hi < lcx.tks.len() {
            let tk = &lcx.tks[self.tsp.hi];
            self.tsp.hi += 1;
            Some(tk)
        } else {
            None
        }
    }

    #[inline(always)]
    fn try_advance<'t>(&mut self, lcx: &'t LexContext) -> Result<&'t Token> {
        self.advance(lcx).ok_or_else(|| SpannedHieLexError {
            tsp: self.tsp,
            err: HieLexError::Eof,
        })
    }
}

/// Immutable lexing methods
impl LexState {
    /// sexp → call | term
    #[inline(always)]
    pub fn maybe_sx(mut self, lcx: &LexContext) -> Result<Sx> {
        self.ignore_ws(lcx);
        let sx = if self.try_peek(lcx)?.kind == TokenKind::ParenOpen {
            self.try_list(lcx).map(Sx::from)?
        } else {
            self.try_symbol(lcx).map(Sx::from)?
        };

        Ok(sx)
    }

    // list → "(" sexp* ")"
    #[inline(always)]
    fn try_list(mut self, lcx: &LexContext) -> Result<List> {
        if self.skip_tk_kind(lcx, TokenKind::ParenOpen).is_none() {
            return Err(SpannedHieLexError {
                tsp: self.tsp,
                err: HieLexError::Unexpected {
                    expected: "list".into(),
                    found: format!("todo err"),
                },
            });
        }

        // proc name
        let mut list = ListBody { operands: vec![] };

        // args
        let mut sub = self.clone();
        sub.tsp.lo = sub.tsp.hi;

        loop {
            sub.ignore_ws(lcx);

            if sub.skip_tk_kind(lcx, TokenKind::ParenClose).is_some() {
                break;
            }

            let sx = match sub.maybe_sx(lcx) {
                Ok(sx) => sx,
                Err(err) => todo!("{}", err),
            };

            sub = LexState::at(sx.tsp().hi);
            list.operands.push(sx);
        }

        // consume the span
        self.tsp.hi = sub.tsp.hi;
        let list = List {
            tsp: TokenSpan {
                lo: self.tsp.lo,
                hi: sub.tsp.hi,
            },
            body: list,
        };

        return Ok(list);
    }

    /// symbol → ident | lit
    #[inline(always)]
    fn try_symbol(mut self, lcx: &LexContext) -> Result<Atom> {
        if let Some(ident) = self.maybe_ident(lcx)? {
            return Ok(Atom::from(ident));
        }

        if let Some(lit) = self.maybe_lit(lcx)? {
            return Ok(Atom::from(lit));
        }

        // consume the span
        let tk = self.try_advance(lcx)?;

        Err(SpannedHieLexError {
            tsp: self.tsp,
            err: HieLexError::Unexpected {
                expected: "symbol".into(),
                found: format!("{:?}", tk),
            },
        })
    }
}

/// Lower-level syntactic items
impl LexState {
    #[inline(always)]
    fn maybe_ident(mut self, lcx: &LexContext) -> Result<Option<Symbol>> {
        let tk = self.try_advance(lcx)?;

        if tk.kind != TokenKind::Ident {
            return Ok(None);
        }

        let sym = Symbol {
            tsp: self.tsp,
            body: SymbolBody::Ident,
        };

        Ok(Some(sym))
    }

    // TODO: support multi-token literals (such as string)
    #[inline(always)]
    fn maybe_lit(mut self, lcx: &LexContext) -> Result<Option<Lit>> {
        let kind = {
            let tk = self.try_advance(lcx)?;
            match LitBody::one_tk(tk.kind) {
                Some(kind) => kind,
                None => return Ok(None),
            }
        };

        let lit = Lit {
            tsp: self.tsp,
            body: kind,
        };

        Ok(Some(lit))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax;

    #[test]
    fn add() -> Result<()> {
        let src = "(+ 1 2)";
        //         0123456
        let file = syntax::tree::from_str(src)?;

        assert_eq!(
            file.sxs,
            vec![Sx::from(List {
                tsp: TokenSpan { lo: 0, hi: 7 },
                body: ListBody {
                    operands: vec![
                        Sx::from(Atom::from(Symbol {
                            tsp: TokenSpan { lo: 1, hi: 2 },
                            body: SymbolBody::Ident,
                        })),
                        Sx::from(Atom::from(Lit {
                            tsp: TokenSpan { lo: 3, hi: 4 },
                            body: LitBody::Num
                        })),
                        Sx::from(Atom::from(Lit {
                            tsp: TokenSpan { lo: 5, hi: 6 },
                            body: LitBody::Num,
                        })),
                    ]
                }
            })],
        );

        Ok(())
    }

    #[test]
    fn nest() -> Result<()> {
        let src = "(+ 1 (* 2 3))";
        //         0 2 4 6 8 0 2
        let file = syntax::tree::from_str(src)?;

        assert_eq!(
            file.sxs,
            vec![Sx::from(List {
                tsp: TokenSpan { lo: 0, hi: 13 },
                body: ListBody {
                    operands: vec![
                        Sx::from(Atom::from(Symbol {
                            tsp: TokenSpan { lo: 1, hi: 2 },
                            body: SymbolBody::Ident,
                        })),
                        Sx::from(Atom::from(Lit {
                            tsp: TokenSpan { lo: 3, hi: 4 },
                            body: LitBody::Num
                        })),
                        Sx::from(List {
                            tsp: TokenSpan { lo: 5, hi: 12 },
                            body: ListBody {
                                operands: vec![
                                    Sx::from(Atom::from(Symbol {
                                        tsp: TokenSpan { lo: 6, hi: 7 },
                                        body: SymbolBody::Ident,
                                    })),
                                    Sx::from(Atom::from(Lit {
                                        tsp: TokenSpan { lo: 8, hi: 9 },
                                        body: LitBody::Num
                                    })),
                                    Sx::from(Atom::from(Lit {
                                        tsp: TokenSpan { lo: 10, hi: 11 },
                                        body: LitBody::Num,
                                    })),
                                ]
                            }
                        })
                    ]
                }
            })],
        );

        Ok(())
    }
}
