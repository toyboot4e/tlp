/*!
`Vec<Token>` → [`TlpFile`]
*/

pub mod ast;

use thiserror::Error;

use crate::{
    lex::{self, LexError, Token, TokenKind},
    parse::ast::*,
    span::ByteSpan,
};

/// `&str` → [`TlpFile`]
pub fn parse(src: &str) -> Result<TlpFile, ParseError> {
    let tks = lex::lex(src)?;
    self::parse_tks(src, &tks)
}

/// `Vec<Token>` → [`TlpFile`]
pub fn parse_tks(src: &str, tks: &[Token]) -> Result<TlpFile, ParseError> {
    FileParse::parse(src, tks)
}

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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct PosSpan {
    lo: usize,
    hi: usize,
}

#[derive(Debug, Clone)]
struct FileParse<'a> {
    src: &'a str,
    tks: &'a [Token],
    sp: PosSpan,
}

impl<'a> FileParse<'a> {
    pub fn parse(src: &'a str, tks: &'a [Token]) -> Result<TlpFile, ParseError> {
        let me = Self {
            src,
            tks,
            sp: PosSpan::default(),
        };

        me.parse_impl()
    }

    fn parse_impl(mut self) -> Result<TlpFile, ParseError> {
        let mut items = vec![];

        while self.sp.lo < self.tks.len() {
            let expr = self.ps_sexp()?;
            items.push(expr);
            // TODO: do we need low boundary
            self.sp.lo = self.sp.hi;
        }

        Ok(TlpFile { items })
    }
}

/// Parse utilities
impl<'a> FileParse<'a> {
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

    fn try_peek(&mut self) -> Result<&Token, ParseError> {
        self.peek().ok_or(ParseError::Eof)
    }
}

/// Higher-level syntactic items
impl<'a> FileParse<'a> {
    /// sexp → call | term
    pub fn ps_sexp(&mut self) -> Result<S, ParseError> {
        if self.try_peek()?.kind == TokenKind::ParenOpen {
            self.ps_call()
        } else {
            self.ps_term()
        }
    }

    fn ps_call(&mut self) -> Result<S, ParseError> {
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

            let exp = self.ps_sexp()?;
            call.args.push(exp);
        }
    }

    /// term → ident | lit
    fn ps_term(&mut self) -> Result<S, ParseError> {
        self.ps_lit()
    }
}

/// Lower-level syntactic items
impl<'a> FileParse<'a> {
    fn ps_lit(&mut self) -> Result<S, ParseError> {
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

    // fn ps_ident(&mut self) -> Result<S, ParseError> {
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
    fn add() -> Result<(), ParseError> {
        let src = "(+ 1 2)";
        //         0 2 4 6
        let file = crate::parse::parse(src)?;

        assert_eq!(
            file.items,
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
    fn recursion() -> Result<(), ParseError> {
        let src = "(+ 1 (* 2 3))";
        //         0 2 4 6 8 0 2
        let file = crate::parse::parse(src)?;

        assert_eq!(
            file.items,
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
