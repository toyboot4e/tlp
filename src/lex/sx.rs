/*!
S-expression tree, spans of tokens in hierarchy
*/

use thiserror::Error;

use crate::{
    lex::{
        self,
        tk::{LexError, Token, TokenKind},
    },
    span::ByteSpan,
};

/// File string tokenized as S-expressions
#[derive(Debug, Clone, Default)]
pub struct FileLex<'a> {
    /// The source file string
    pub src: &'a str,
    /// Items just under the file
    pub sxs: Vec<Sx>,
}

/// Span of [`Token`] s in range `(lo, hi])` referred to as `tsp`
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct TokenSpan {
    pub lo: usize,
    pub hi: usize,
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
/// * [`Atom`] = `Symbol` | [`Lit`]
/// * Symbol = ValSymbol | FnSymbol | Keyword
#[derive(Debug, Clone, PartialEq)]
pub enum Sx {
    Atom(Atom),
    List(List),
}

// S-expression = List | Atom
enum_from!(Sx, List, Atom);

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Lit(Lit),
    Symbol(Symbol),
}

// Atom = Lit | Symbol
enum_from!(Atom, Lit, Symbol);

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

pub type Result<T, E = ParseError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
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

/// Creates [`FileLex`] from `&str`
pub fn from_str<'a>(src: &'a str) -> Result<FileLex<'a>> {
    let tks = lex::tk::from_str(src)?;
    self::from_tks(src, &tks)
}

/// Creates [`FileLex`] from `&str` and `&[Token]`
pub fn from_tks<'s, 't>(src: &'s str, tks: &'t [Token]) -> Result<FileLex<'s>>
where
    's: 't,
{
    ParseContext::parse(src, tks)
}

/// Immutable shared data for parse functions
///
/// Referred to as `pcx`.
#[derive(Debug, Clone)]
struct ParseContext<'s, 't>
where
    's: 't,
{
    src: &'s str,
    tks: &'t [Token],
}

/// Variable parse state for a sub structure of source code
///
/// Referred to as `pcx`.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct ParseState {
    tsp: TokenSpan,
}

impl<'s, 't> ParseContext<'s, 't>
where
    's: 't,
{
    pub fn parse(src: &'s str, tks: &'t [Token]) -> Result<FileLex<'s>> {
        let pcx = Self { src, tks };
        pcx.parse_impl()
    }

    fn parse_impl(self) -> Result<FileLex<'s>> {
        let mut state = ParseState {
            tsp: TokenSpan { lo: 0, hi: 0 },
        };
        let mut sxs = vec![];

        while state.tsp.lo < self.tks.len() {
            let sx = state.maybe_sx(&self)?;
            sxs.push(sx);
        }

        Ok(FileLex { src: self.src, sxs })
    }
}

/// Parse utilities
impl ParseState {
    /// Advances the high end of the span
    #[inline(always)]
    fn skip_ws(&mut self, pcx: &ParseContext) -> Option<()> {
        self.skip_tk_kind(pcx, TokenKind::Ws)
    }

    /// Advances the high end of the span when a peeked token matches to given kind
    ///
    /// FIXME: EoF safety
    #[inline(always)]
    fn skip_tk_kind(&mut self, pcx: &ParseContext, kind: TokenKind) -> Option<()> {
        if pcx.tks[self.tsp.hi].kind == kind {
            self.tsp.hi += 1;
            Some(())
        } else {
            None
        }
    }

    /// Advances both the high end and the low end of the span
    #[inline(always)]
    fn ignore_ws(&mut self, pcx: &ParseContext) -> Option<()> {
        self.ignore_tk_kind(pcx, TokenKind::Ws)
    }

    /// Advances both the high end and the low end of the span when a peeked token matches to given
    /// kind
    ///
    /// FIXME: EoF safety
    #[inline(always)]
    fn ignore_tk_kind(&mut self, pcx: &ParseContext, kind: TokenKind) -> Option<()> {
        if pcx.tks[self.tsp.hi].kind == kind {
            self.tsp.hi += 1;
            self.tsp.lo = self.tsp.hi;
            Some(())
        } else {
            None
        }
    }

    #[inline(always)]
    fn peek<'t>(&mut self, pcx: &'t ParseContext) -> Option<&'t Token> {
        if self.tsp.hi < pcx.tks.len() {
            Some(&pcx.tks[self.tsp.hi])
        } else {
            None
        }
    }

    #[inline(always)]
    fn try_peek<'t>(&mut self, pcx: &'t ParseContext) -> Result<&'t Token> {
        self.peek(pcx).ok_or(ParseError::Eof)
    }

    #[inline(always)]
    fn advance<'t>(&mut self, pcx: &'t ParseContext) -> Option<&'t Token> {
        if self.tsp.hi < pcx.tks.len() {
            let tk = &pcx.tks[self.tsp.hi];
            self.tsp.hi += 1;
            Some(tk)
        } else {
            None
        }
    }

    #[inline(always)]
    fn try_advance<'t>(&mut self, pcx: &'t ParseContext) -> Result<&'t Token> {
        self.advance(pcx).ok_or(ParseError::Eof)
    }
}

/// Higher-level syntactic items
impl ParseState {
    /// sexp → call | term
    #[inline(always)]
    pub fn maybe_sx(&mut self, pcx: &ParseContext) -> Result<Sx> {
        self.ignore_ws(pcx);
        let sx = if self.try_peek(pcx)?.kind == TokenKind::ParenOpen {
            self.try_list(pcx).map(Sx::from)?
        } else {
            self.try_symbol(pcx).map(Sx::from)?
        };

        Ok(sx)
    }

    // list → "(" sexp* ")"
    #[inline(always)]
    fn try_list(&mut self, pcx: &ParseContext) -> Result<List> {
        if self.skip_tk_kind(pcx, TokenKind::ParenOpen).is_none() {
            return Err(ParseError::Unexpected {
                expected: "list".into(),
                found: format!("todo err"),
            });
        }

        // proc name
        let mut list = ListBody { operands: vec![] };

        // args
        let mut sub = self.clone();

        loop {
            sub.tsp.lo = sub.tsp.hi;
            sub.ignore_ws(pcx);

            if sub.skip_tk_kind(pcx, TokenKind::ParenClose).is_some() {
                break;
            }

            let sx = match sub.maybe_sx(pcx) {
                Ok(sx) => sx,
                Err(err) => todo!("{}", err),
            };

            list.operands.push(sx);
        }

        // consume the span
        self.tsp.hi = sub.tsp.hi;
        let list = List {
            tsp: self.tsp,
            body: list,
        };

        self.tsp.lo = self.tsp.hi;

        return Ok(list);
    }

    /// symbol → ident | lit
    #[inline(always)]
    fn try_symbol(&mut self, pcx: &ParseContext) -> Result<Atom> {
        println!("sym: {:?}", self);
        if let Some(ident) = self.maybe_ident(pcx)? {
            return Ok(Atom::from(ident));
        }

        if let Some(lit) = self.maybe_lit(pcx)? {
            return Ok(Atom::from(lit));
        }

        // consume the span
        let tk = self.try_advance(pcx)?;
        self.tsp.lo = self.tsp.hi;

        Err(ParseError::Unexpected {
            expected: "symbol".into(),
            found: format!("{:?}", tk),
        })
    }
}

/// Lower-level syntactic items
impl ParseState {
    #[inline(always)]
    fn maybe_ident(&mut self, pcx: &ParseContext) -> Result<Option<Symbol>> {
        let tk = self.try_peek(pcx)?;

        if tk.kind != TokenKind::Ident {
            return Ok(None);
        }

        self.tsp.hi += 1;

        let sym = Symbol {
            tsp: self.tsp,
            body: SymbolBody::Ident,
        };

        self.tsp.lo = self.tsp.hi;
        Ok(Some(sym))
    }

    // TODO: support multi-token literals (such as string)
    #[inline(always)]
    fn maybe_lit(&mut self, pcx: &ParseContext) -> Result<Option<Lit>> {
        let kind = {
            let tk = self.try_peek(pcx)?;
            match LitBody::one_tk(tk.kind) {
                Some(kind) => kind,
                None => return Ok(None),
            }
        };

        self.tsp.hi += 1;

        let lit = Lit {
            tsp: self.tsp,
            body: kind,
        };

        self.tsp.lo = self.tsp.hi;
        Ok(Some(lit))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lex;

    #[test]
    fn add() -> Result<()> {
        let src = "(+ 1 2)";
        //         0123456
        let file = lex::sx::from_str(src)?;

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

    // #[test]
    // fn nest() -> Result<()> {
    //     let src = "(+ 1 (* 2 3))";
    //     //         0 2 4 6 8 0 2
    //     let file = lex::sx::from_str(src)?;

    //     assert_eq!(
    //         file.sxs,
    //         vec![Sx::from(List {
    //             sp: TokenSpan { lo: 0, hi: 13 },
    //             body: ListBody {
    //                 operands: vec![
    //                     Sx::from(Atom::from(Symbol {
    //                         sp: TokenSpan { lo: 1, hi: 2 },
    //                         body: SymbolBody::Ident,
    //                     })),
    //                     Sx::from(Atom::from(Lit {
    //                         sp: TokenSpan { lo: 3, hi: 4 },
    //                         body: LitBody::Num
    //                     })),
    //                     Sx::from(List {
    //                         sp: TokenSpan { lo: 5, hi: 12 },
    //                         body: ListBody {
    //                             operands: vec![
    //                                 Sx::from(Atom::from(Symbol {
    //                                     sp: TokenSpan { lo: 6, hi: 7 },
    //                                     body: SymbolBody::Ident,
    //                                 })),
    //                                 Sx::from(Atom::from(Lit {
    //                                     sp: TokenSpan { lo: 8, hi: 9 },
    //                                     body: LitBody::Num
    //                                 })),
    //                                 Sx::from(Atom::from(Lit {
    //                                     sp: TokenSpan { lo: 10, hi: 11 },
    //                                     body: LitBody::Num,
    //                                 })),
    //                             ]
    //                         }
    //                     })
    //                 ]
    //             }
    //         })],
    //     );

    //     Ok(())
    // }
}
