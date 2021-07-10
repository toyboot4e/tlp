/*!
Syntactic validation of AST
*/

use thiserror::Error;

use crate::syntax::{ast::data::*, cst::data::*, span::*};

#[derive(Debug, Clone, Error)]
pub enum SyntaxError {
    #[error("Missing token {kind:?}")]
    MissingToken { at: TextPos, kind: SyntaxKind },
    #[error("Unexpected {found}, expected {expected}")]
    Unexpected {
        at: TextPos,
        found: String,
        expected: String,
    },
    #[error("Missing function name for {f}")]
    FnName { f: SyntaxNode },
    #[error("Missing function parameters for {f}")]
    FnParamsMissing { f: SyntaxNode },
}

/// Range to span
fn r2s(rng: rowan::TextRange) -> ByteSpan {
    let (lo, hi) = (rng.start().into(), rng.end().into());
    ByteSpan { lo, hi }
}

impl SyntaxError {
    pub fn span(&self) -> ByteSpan {
        match self {
            Self::MissingToken { at, .. } => ByteSpan::at(*at),
            Self::Unexpected { at, .. } => ByteSpan::at(*at),
            Self::FnName { f } => self::r2s(f.text_range()),
            Self::FnParamsMissing { f } => self::r2s(f.text_range()),
        }
    }
}

pub trait Validate {
    fn validate(&self, errs: &mut Vec<SyntaxError>);
}

fn lparen(syn: SyntaxNode, errs: &mut Vec<SyntaxError>) {
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
        for form in self.items() {
            form.validate(errs);
        }
    }
}

impl Validate for Form {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        if let Some(defn) = self.as_def_fn() {
            defn.validate(errs);
            return;
        }

        if let Some(call) = self.as_call() {
            call.validate(errs);
            return;
        }

        if let Some(atom) = self.as_atom() {
            atom.validate(errs);
            return;
        }

        unreachable!("Not a form: {}", self.syn);
    }
}

impl Validate for Call {
    fn validate(&self, errs: &mut Vec<SyntaxError>) {
        // TODO: validate each argument
    }
}

impl Validate for DefFn {
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
                    at: e.text_range().start().into(),
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
