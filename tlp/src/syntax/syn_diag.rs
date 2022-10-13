//! Syntax diagnostics

use base::{jar::InputFile, span::Span, BaseDb};

use crate::{
    syntax::cst::{lex::LexError, ParseError},
    util::diag::{self, Diagnostic},
};

impl diag::Diagnostic for LexError {
    fn code(&self) -> Option<&'static str> {
        Some(match self {
            Self::UnterminatedString { .. } => "E0000",
        })
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> String {
        match self {
            Self::UnterminatedString { .. } => "unterminated string",
        }
        .to_string()
    }
}

impl diag::Diagnostic for ParseError {
    fn code(&self) -> Option<&'static str> {
        Some(match self {
            ParseError::LexError { err } => return err.code(),
            ParseError::Unexpected { .. } => "E0010",
            ParseError::UnterminatedString { .. } => "E0011",
            ParseError::PathNotEndWithIdent { .. } => "E0012",
        })
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> String {
        format!("{}", self)
    }
}

impl LexError {
    pub fn render<'a>(&'a self, db: &'a dyn BaseDb, input_file: InputFile) -> diag::Render<'a> {
        match self {
            LexError::UnterminatedString { span } => {
                let src_context = "".to_string();
                let msg_span = diag::MsgSpan::new(*span, self.msg());
                diag::render_single_msg(db, self, input_file, src_context, msg_span)
            }
        }
    }
}

impl ParseError {
    pub fn render<'a>(&'a self, db: &'a dyn BaseDb, input_file: InputFile) -> diag::Render<'a> {
        match self {
            ParseError::LexError { err } => err.render(db, input_file),
            ParseError::Unexpected {
                at,
                expected,
                found,
            } => todo!(),
            ParseError::UnterminatedString { sp } => todo!(),
            ParseError::PathNotEndWithIdent { sp } => todo!(),
        }
    }
}
