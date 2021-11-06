/*!
Syntactic validation of AST
*/

use thiserror::Error;

use crate::syntax::{ast::*, cst::*, span::*};

#[derive(Debug, Clone, Error)]
pub enum SyntaxError {
    #[error("Missing token {kind:?}")]
    MissingToken { at: TextPos, kind: SyntaxKind },
    #[error("Expected `{expected}`, found `{found}`")]
    Unexpected {
        sp: ByteSpan,
        found: String,
        expected: String,
    },
    #[error("Missing function name for {f}")]
    FnName { f: SyntaxNode },
    #[error("Missing function parameters for {f}")]
    FnParamsMissing { f: SyntaxNode },
}

impl SyntaxError {
    pub fn span(&self) -> ByteSpan {
        match self {
            Self::MissingToken { at, .. } => ByteSpan::at(*at),
            Self::Unexpected { sp, .. } => *sp,
            Self::FnName { f } => f.text_range().into(),
            Self::FnParamsMissing { f } => f.text_range().into(),
        }
    }
}

pub trait Validate {
    fn validate(&self, errs: &mut Vec<SyntaxError>);
}

fn lparen(syn: SyntaxNode, _errs: &mut Vec<SyntaxError>) {
    let tk = match syn.first_token() {
        Some(tk) => tk,
        None => return,
    };

    if tk.kind() != SyntaxKind::LParen {
        unreachable!("List can't be without left paren");
    }
}

fn rparen(syn: SyntaxNode, errs: &mut Vec<SyntaxError>) {
    let tks = syn
        .children_with_tokens()
        .filter_map(|elem| elem.into_token());

    // TODO: reversible tokens without vec?
    let tks = tks.collect::<Vec<_>>();

    let tk = tks
        .iter()
        .rev()
        .skip_while(|tk| tk.kind() == SyntaxKind::Ws)
        .next();

    match tk {
        Some(tk) if tk.kind() == SyntaxKind::RParen => {}
        Some(tk) => {
            errs.push(SyntaxError::MissingToken {
                at: tk.text_range().start().into(),
                kind: SyntaxKind::RParen,
            });
        }
        None => {
            unreachable!("invalid node");
        }
    }
}

impl Validate for Document {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        for form in self.item_nodes() {
            form.validate(errs);
        }
    }
}

impl Validate for Form {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        match self {
            Self::DefProc(proc) => {
                proc.validate(errs);
            }
            Self::Call(_call) => {}
            Self::Atom(atom) => {
                atom.validate(errs);
            }
        }
    }
}

impl Validate for Call {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
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
        let mut elems = self.syn.children_with_tokens().filter(|elem| {
            !matches!(
                elem.kind(),
                SyntaxKind::Ws | SyntaxKind::Comment | SyntaxKind::LParen | SyntaxKind::RParen
            )
        });

        for e in elems {
            if e.kind() != SyntaxKind::Ident {
                errs.push(SyntaxError::Unexpected {
                    sp: e.text_range().into(),
                    found: format!("{:?}", e.kind()),
                    expected: format!("{:?}", SyntaxKind::Ident),
                });
            }
        }
    }
}

impl Validate for Atom {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        //
    }
}
