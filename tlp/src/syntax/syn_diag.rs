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
            ParseError::UnclosedParentheses { .. } => "E0010",
            ParseError::UnexpectedToken { .. } => "E0011",
            ParseError::UnexpectedEof { .. } => "E0012",
            ParseError::PathNotEndWithIdent { .. } => "E0013",
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
            ParseError::UnexpectedToken { expected: _, found } => {
                let src_context = "".to_string();
                let msg_span = diag::MsgSpan::new(*found, self.msg());

                diag::render_single_msg(db.base(), self, input_file, src_context, msg_span)
            }
            ParseError::UnexpectedEof { expected: _ } => {
                let src_context = "".to_string();

                // FIXME: refer to EoF
                let src_len = input_file.source_text(db).len();
                let span = Span {
                    // FIXME: unsafe
                    start: (src_len - 1).into(),
                    end: src_len.into(),
                };

                let msg_span = diag::MsgSpan::new(span, self.msg());

                diag::render_single_msg(db.base(), self, input_file, src_context, msg_span)
            }
            ParseError::UnclosedParentheses { span } => {
                let src_context = "".to_string();
                let msg_span = diag::MsgSpan::new(*span, self.msg());
                diag::render_single_msg(db, self, input_file, src_context, msg_span)
            }
            ParseError::PathNotEndWithIdent { span } => todo!(),
        }
    }
}
