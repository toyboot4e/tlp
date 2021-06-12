/*!
`Vec<Token>` -> `Vec<S>`
*/

mod ast;

pub use ast::*;

use thiserror::Error;

use crate::{
    lex::{LexError, Token, TokenKind},
    span::ByteSpan,
};

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
}

pub fn lex_and_parse(src: &str) -> Result<Ast, ParseError> {
    let tks = crate::lex::lex(src)?;
    self::parse(src, &tks)
}

pub fn parse(src: &str, tks: &[Token]) -> Result<Ast, ParseError> {
    Parser::new(src, tks).parse()
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct PosSpan {
    lo: usize,
    hi: usize,
}

#[derive(Debug, Clone)]
struct Parser<'a> {
    src: &'a str,
    tks: &'a [Token],
    sp: PosSpan,
    ast: Ast,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, tks: &'a [Token]) -> Self {
        Self {
            src,
            tks,
            sp: PosSpan::default(),
            ast: Ast::default(),
        }
    }

    pub fn parse(mut self) -> Result<Ast, ParseError> {
        while self.sp.lo < self.tks.len() {
            self.ps_sexp()?;
        }

        Ok(self.ast)
    }

    pub fn ps_sexp(&mut self) -> Result<(), ParseError> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn star() -> Result<(), ParseError> {
        let src = "(* 1 3)";
        //         0 2 4 6
        let ast = lex_and_parse(src)?;

        // assert_eq!(
        //
        // );

        Ok(())
    }
}
