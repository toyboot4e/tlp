//! Compiler (HIR → bytecode)

use thiserror::Error;

use crate::{syntax::ast::*, vm::code::Chunk};

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("Not an item")]
    NotAnItem { kind: FormKind },
    #[error("Unexisting method call")]
    UnexistingMethodCall,
}

// pub struct Manifest {}

pub trait Compile {
    fn compile(&self, code: &mut Chunk) -> Result<()>;
}

pub fn compile(doc: Document) -> (Chunk, Vec<CompileError>) {
    let mut code = Chunk::new();
    let mut errs = vec![];

    for form in doc.item_nodes() {
        let kind = form.kind();

        match kind {
            FormKind::Call(call) => {
                continue;
            }
            _ => errs.push(CompileError::NotAnItem { kind }),
        }
    }

    (code, errs)
}
