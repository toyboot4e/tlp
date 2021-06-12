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
    let ast = self::parse(&tks)?;
    Ok(ast)
}

pub fn parse(_tks: &[Token]) -> Result<Ast, ParseError> {
    todo!()
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
