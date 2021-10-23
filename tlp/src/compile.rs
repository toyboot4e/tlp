/*!
Compiler (HIR → bytecode)
*/

use thiserror::Error;

use crate::{syntax::ast::data::*, vm::data::Chunk};

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("Invalid item in file")]
    InvalidItem,
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
        if let Form::Call(call) = form {
            //
        }

        errs.push(CompileError::InvalidItem);
    }

    (code, errs)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::vm::runtime::Vm;

    fn compile(src: &str) -> Vm {
        let (doc, errs) = crate::syntax::ast::parse(src).into_tuple();
        assert!(errs.is_empty());

        let (code, errs) = crate::compile::compile(doc);
        assert!(errs.is_empty());

        Vm::new(code)
    }

    #[test]
    fn arithmetics() {
        let src = "((64.0 - 32.0) / 16.0)";
        return;

        let mut vm = self::compile(src);

        vm.run().unwrap();
        assert_eq!(Some(&2.0), vm.stack().last());
    }
}
