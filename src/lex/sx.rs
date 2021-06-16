/*!
S-expression tree, spans in a hierarchy
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
    pub sxs: Vec<S>,
}

/// S-expression is a list or an atom
///
/// # Syntax
///
/// * Atom = Symbol | Literal
/// * Symbol = ValSymbol | FnSymbol | Keyword
#[derive(Debug, Clone, PartialEq)]
pub enum S {
    Lit(Lit),
    Call(Call),
    // Var(Var),
}

macro_rules! from_ty {
    ($($ty:ident),* $(,)?) => {
        $(
            impl From<$ty> for S {
                fn from(x: $ty) -> S {
                    S::$ty(x)
                }
            }
        )*
    };
}

from_ty!(Lit, Call,);

#[derive(Debug, Clone, PartialEq)]
pub struct Lit {
    pub kind: LitKind,
    pub sp: ByteSpan,
}

impl Lit {
    /// Creates [`Lit`] WITHOUT ensuring it's valid literal
    pub fn from_tk(tk: &Token) -> Option<Self> {
        Some(Self {
            kind: LitKind::from_tk_kind(tk.kind)?,
            sp: tk.sp,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum LitKind {
    Nil,
    Bool,
    Str,
    Num,
}

impl LitKind {
    /// Creates [`LitKind`] WITHOUT ensuring it's valid literal
    pub fn from_tk_kind(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Ws => None,
            TokenKind::ParenOpen => None,
            TokenKind::ParenClose => None,
            TokenKind::True => Some(LitKind::Bool),
            TokenKind::False => Some(LitKind::Bool),
            TokenKind::Nil => Some(LitKind::Nil),
            TokenKind::Num => Some(LitKind::Num),
            TokenKind::StrEnclosure => None,
            TokenKind::StrContent => Some(LitKind::Str),
            TokenKind::Ident => None,
        }
    }
}

/// Function call
#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub ident: ByteSpan,
    pub args: Vec<S>,
}

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
    FileParse::parse(src, tks)
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct PosSpan {
    lo: usize,
    hi: usize,
}

#[derive(Debug, Clone)]
struct FileParse<'s, 't>
where
    's: 't,
{
    src: &'s str,
    tks: &'t [Token],
    sp: PosSpan,
}

impl<'s, 't> FileParse<'s, 't>
where
    's: 't,
{
    pub fn parse(src: &'s str, tks: &'t [Token]) -> Result<FileLex<'s>> {
        let me = Self {
            src,
            tks,
            sp: PosSpan::default(),
        };

        me.parse_impl()
    }

    fn parse_impl(mut self) -> Result<FileLex<'s>> {
        let mut sxs = vec![];

        while self.sp.lo < self.tks.len() {
            let expr = self.maybe_sx()?;
            sxs.push(expr);
            // TODO: do we need low boundary
            self.sp.lo = self.sp.hi;
        }

        Ok(FileLex { src: self.src, sxs })
    }
}

/// Parse utilities
impl<'s, 't> FileParse<'s, 't>
where
    's: 't,
{
    fn skip_ws(&mut self) -> Option<()> {
        self.skip_tk_kind(TokenKind::Ws)
    }

    /// Just skip the expected kind of token
    // FIXME: EoF safety
    fn skip_tk_kind(&mut self, kind: TokenKind) -> Option<()> {
        if self.tks[self.sp.hi].kind == kind {
            self.sp.hi += 1;
            Some(())
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.sp.hi < self.tks.len() {
            self.skip_ws();
            Some(&self.tks[self.sp.hi])
        } else {
            None
        }
    }

    fn try_peek(&mut self) -> Result<&Token> {
        self.peek().ok_or(ParseError::Eof)
    }
}

/// Higher-level syntactic items
impl<'s, 't> FileParse<'s, 't>
where
    's: 't,
{
    /// sexp → call | term
    pub fn maybe_sx(&mut self) -> Result<S> {
        if self.try_peek()?.kind == TokenKind::ParenOpen {
            self.maybe_call()
        } else {
            self.maybe_term()
        }
    }

    fn maybe_call(&mut self) -> Result<S> {
        self.skip_tk_kind(TokenKind::ParenOpen);

        // proc name
        let mut call = {
            self.skip_tk_kind(TokenKind::Ident);

            Call {
                ident: self.tks[self.sp.hi - 1].sp,
                args: Default::default(),
            }
        };

        // args
        loop {
            self.try_peek()?;
            if self.skip_tk_kind(TokenKind::ParenClose).is_some() {
                return Ok(S::from(call));
            }

            let exp = self.maybe_sx()?;
            call.args.push(exp);
        }
    }

    /// term → ident | lit
    fn maybe_term(&mut self) -> Result<S> {
        self.parse_lit()
    }
}

/// Lower-level syntactic items
impl<'s, 't> FileParse<'s, 't>
where
    's: 't,
{
    fn parse_lit(&mut self) -> Result<S> {
        let lit = {
            let tk = self.try_peek()?;
            Lit::from_tk(tk).ok_or_else(|| ParseError::Unexpected {
                expected: "literal".to_string(),
                found: format!("{:?}", tk),
            })?
        };

        self.sp.hi += 1;
        Ok(S::from(lit))
    }

    // fn ps_ident(&mut self) -> Result<S> {
    //     let ident = {
    //         let tk = self.try_peek()?;
    //         Lit::from_tk(tk).ok_or_else(|| ParseError::Unexpected {
    //             expected: "literal".to_string(),
    //             found: format!("{:?}", tk),
    //         })?
    //     };
    //
    //     self.sp.hi += 1;
    //     Ok(S::from(ident))
    // }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn add() -> Result<()> {
        let src = "(+ 1 2)";
        //         0 2 4 6
        let file = crate::lex::sx::from_str(src)?;

        assert_eq!(
            file.sxs,
            vec![S::from(Call {
                ident: ByteSpan { lo: 1, hi: 2 },
                args: vec![
                    S::from(Lit {
                        kind: LitKind::Num,
                        sp: ByteSpan { lo: 3, hi: 4 },
                    }),
                    S::from(Lit {
                        kind: LitKind::Num,
                        sp: ByteSpan { lo: 5, hi: 6 },
                    })
                ]
            })]
        );

        Ok(())
    }

    #[test]
    fn recursion() -> Result<()> {
        let src = "(+ 1 (* 2 3))";
        //         0 2 4 6 8 0 2
        let file = crate::lex::sx::from_str(src)?;

        assert_eq!(
            file.sxs,
            vec![S::from(Call {
                ident: ByteSpan { lo: 1, hi: 2 },
                args: vec![
                    S::from(Lit {
                        kind: LitKind::Num,
                        sp: ByteSpan { lo: 3, hi: 4 },
                    }),
                    S::from(Call {
                        ident: ByteSpan { lo: 6, hi: 7 },
                        args: vec![
                            S::from(Lit {
                                kind: LitKind::Num,
                                sp: ByteSpan { lo: 8, hi: 9 },
                            }),
                            S::from(Lit {
                                kind: LitKind::Num,
                                sp: ByteSpan { lo: 10, hi: 11 },
                            }),
                        ]
                    })
                ]
            })]
        );

        Ok(())
    }
}
