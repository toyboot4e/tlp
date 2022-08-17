//! Compiler (HIR → bytecode)

pub mod scope;

use thiserror::Error;

use crate::{
    syntax::ast,
    vm::code::{Chunk, OpCode},
};

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("Unexpected form: {form:?}")]
    UnexpectedForm { form: ast::Form },
    #[error("Unexisting method call")]
    UnexistingMethodCall,
}

// pub struct Manifest {}

pub trait Compile {
    fn compile(&self, code: &mut Chunk) -> Result<()>;
}

//
pub struct Scope {
    pub len: usize,
}

// TODO: compile `hir_def`, not `ast`
pub fn compile(doc: ast::Document) -> (Chunk, Vec<CompileError>) {
    let mut chunk = Chunk::new();
    let mut errs = vec![];

    for form in doc.item_nodes() {
        self::compile_form(&mut chunk, &mut errs, &form);
    }

    (chunk, errs)
}

/// Dummy impl
fn compile_form(chunk: &mut Chunk, errs: &mut Vec<CompileError>, form: &ast::Form) {
    match form {
        ast::Form::Call(call) => {
            let path = call.path();
            // TODO: support path
            let ident = path.components().next().unwrap();

            match ident.text() {
                "+" | "-" | "*" | "/" => {
                    let mut args = call.args();

                    let lhs = args.next().unwrap();
                    compile_form(chunk, errs, &lhs);

                    let rhs = args.next().unwrap();
                    compile_form(chunk, errs, &rhs);

                    let op = self::to_oper(ident.text()).unwrap();
                    chunk.push_code(op);
                }
                _ => {
                    todo!("{:?}", call);
                }
            };
        }
        ast::Form::Let(_let_) => {
            // let pat = let_.pat().unwrap();
            // TODO: resolve
            todo!()
        }
        ast::Form::Literal(lit) => match lit.kind() {
            ast::LiteralKind::Num(x) => {
                let x: f64 = x.text().parse().unwrap();
                let i = chunk.push_const(x);
                chunk.push_ix_u8(i as u8);
            }
            _ => panic!(),
        },
        _ => errs.push(CompileError::UnexpectedForm { form: form.clone() }),
    }
}

fn to_oper(s: &str) -> Option<OpCode> {
    Some(match s {
        "+" => OpCode::OpAdd,
        "-" => OpCode::OpSub,
        "*" => OpCode::OpMul,
        "/" => OpCode::OpDiv,
        _ => return None,
    })
}
