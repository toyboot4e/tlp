//! Compiler (HIR → bytecode)

use thiserror::Error;

use crate::{
    syntax::ast,
    vm::code::{Chunk, OpCode},
};

type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("Unexpected kind: {kind:?}")]
    UnexpectedKind { kind: ast::FormKind },
    #[error("Unexisting method call")]
    UnexistingMethodCall,
}

// pub struct Manifest {}

pub trait Compile {
    fn compile(&self, code: &mut Chunk) -> Result<()>;
}

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
    let kind = form.kind();

    match kind {
        ast::FormKind::Call(call) => match call.name_tk().unwrap().text() {
            "+" | "-" | "*" | "/" => {
                let mut args = call.arg_forms();

                let lhs = args.next().unwrap();
                compile_form(chunk, errs, &lhs);

                let rhs = args.next().unwrap();
                compile_form(chunk, errs, &rhs);

                let op = self::to_oper(call.name_tk().unwrap().text()).unwrap();
                chunk.push_code(op);
            }
            _ => {
                todo!("{:?}", call);
            }
        },
        ast::FormKind::Let(let_) => {
            let pat = let_.pat().unwrap();
            todo!()
        }
        ast::FormKind::Literal(lit) => match lit.kind() {
            ast::LiteralKind::Num(x) => {
                let x: f64 = x.text().parse().unwrap();
                let i = chunk.push_const(x);
                chunk.push_ix_u8(i as u8);
            }
            _ => panic!(),
        },
        _ => errs.push(CompileError::UnexpectedKind { kind }),
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
