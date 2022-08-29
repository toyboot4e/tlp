//! Syntactic validation of AST

use thiserror::Error;

use crate::{
    base::span::{Offset, Span},
    syntax::{ast::*, cst::*},
};

#[derive(Debug, Clone, Error)]
pub enum SyntaxError {
    #[error("Missing token {kind:?}")]
    MissingToken { at: Offset, kind: SyntaxKind },
    #[error("Expected `{expected}`, found `{found}`")]
    Unexpected {
        sp: Span,
        found: String,
        expected: String,
    },
    #[error("Missing function name for {f}")]
    FnName { f: SyntaxNode },
    #[error("Missing function parameters for {f}")]
    FnParamsMissing { f: SyntaxNode },
}

impl SyntaxError {
    pub fn span(&self) -> Span {
        match self {
            Self::MissingToken { at, .. } => Span {
                start: *at,
                end: (*at + 1u32).into(),
            },
            Self::Unexpected { sp, .. } => *sp,
            Self::FnName { f } => Span::from_rowan_range(f.text_range()),
            Self::FnParamsMissing { f } => Span::from_rowan_range(f.text_range()),
        }
    }
}

pub trait Validate {
    fn validate(&self, errs: &mut Vec<SyntaxError>);
}

impl Validate for Document {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        for item in self.items() {
            item.validate(errs);
        }
    }
}

impl Validate for Item {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        match self {
            Item::DefProc(proc) => {
                proc.validate(errs);
            }
        }
    }
}

impl Validate for Expr {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        match self {
            // TODO: validate
            Expr::Call(_call) => {}
            Expr::Let(_let) => {}
            Expr::Path(_path) => {}
            Expr::Literal(lit) => {
                lit.validate(errs);
            }
            Expr::Block(_block) => {}
        }
    }
}

impl Validate for Call {
    fn validate(&self, _errs: &mut Vec<SyntaxError>) {
        // TODO: validate each argument
    }
}

impl Validate for DefProc {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        let mut c = self
            .syn
            .children_with_tokens()
            .filter(|elem| elem.kind() != SyntaxKind::Ws);

        assert_eq!(c.next().unwrap().kind(), SyntaxKind::LParen);

        if c.next().unwrap().kind() != SyntaxKind::Ident {
            errs.push(SyntaxError::FnName {
                f: self.syn.clone(),
            });
        }

        let params = match self.params() {
            Some(params) => params,
            None => {
                errs.push(SyntaxError::FnParamsMissing {
                    f: self.syn.clone(),
                });
                return;
            }
        };

        params.validate(errs);
    }
}

impl Validate for Params {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        let elems = self.syn.children_with_tokens().filter(|elem| {
            !matches!(
                elem.kind(),
                SyntaxKind::Ws | SyntaxKind::Comment | SyntaxKind::LParen | SyntaxKind::RParen
            )
        });

        for e in elems {
            if e.kind() != SyntaxKind::Ident {
                errs.push(SyntaxError::Unexpected {
                    sp: Span::from_rowan_range(e.text_range()),
                    found: format!("{:?}", e.kind()),
                    expected: format!("{:?}", SyntaxKind::Ident),
                });
            }
        }
    }
}

impl Validate for Literal {
    fn validate(&self, _errs: &mut Vec<SyntaxError>) {
        //
    }
}
