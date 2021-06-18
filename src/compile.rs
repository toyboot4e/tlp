/*!
Bytecode and compiler for it
*/

pub mod chunk;

use thiserror::Error;

use crate::syntax::{lex::Token, tree::*};

use self::chunk::*;

pub type Result<T, E = CompileError> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("lex error: {err}")]
    HieLexError {
        #[from]
        err: HieLexError,
    },
    #[error("expected callable, found {tks:?}")]
    NotCallable { tks: Vec<Token> },
    #[error("not a root item: {found:?}")]
    NotRootItem { found: Sx },
}

/// Compiles the lexed file as valid one
pub fn from_str(s: &str) -> Result<ChunkData> {
    let file = crate::syntax::tree::from_str(s).map_err(|e| e.err)?;
    self::from_file(&file)
}

/// Compiles the lexed file as valid one
pub fn from_file(file: &FileLex) -> Result<ChunkData> {
    let mut chunk = ChunkData::new();

    for sx in &file.sxs {
        self::compile_root_item(&mut chunk, file, sx)?;
    }

    Ok(chunk)
}

// struct CompileContext<'c, 's, 'f> {
//     chunk: &'c mut ChunkData,
//     file: &'f FileLex<'s>,
// }

/// Similar to [`compile_sx`], but forbids some kind of items
fn compile_root_item(chunk: &mut ChunkData, file: &FileLex, sx: &Sx) -> Result<()> {
    match sx {
        Sx::Atom(_atom) => return Err(CompileError::NotRootItem { found: sx.clone() }),
        Sx::List(list) => {
            self::compile_list(chunk, file, list)?;
        }
    }

    Ok(())
}

fn compile_list(chunk: &mut ChunkData, file: &FileLex, list: &List) -> Result<()> {
    assert!(!list.body.operands.is_empty(), "handle unit");

    let first = &list.body.operands[0];
    let args = &list.body.operands[1..];

    let call = {
        match first {
            Sx::Atom(atom) => match atom {
                Atom::Lit(lit) => {
                    return Err(CompileError::NotCallable {
                        tks: lit.tsp.slice(&file.tks).to_vec(),
                    });
                }
                Atom::Symbol(symbol) => match symbol.body {
                    SymbolBody::Ident => {
                        let tk = &symbol.tsp.slice(&file.tks)[0];
                        tk.sp.slice(&file.src)
                    }
                },
            },
            Sx::List(list) => {
                return Err(CompileError::NotCallable {
                    tks: list.tsp.slice(&file.tks).to_owned(),
                });
            }
        }
    };

    match call {
        "+" => self::compile_bin_op(chunk, file, args, OpCode::OpAdd),
        "-" if args.len() == 1 => {
            chunk.push_code(OpCode::OpNegate);
            for sx in args {
                self::compile_sx(chunk, file, sx)?;
            }
            Ok(())
        }
        "-" => self::compile_bin_op(chunk, file, args, OpCode::OpSub),
        "*" => self::compile_bin_op(chunk, file, args, OpCode::OpMul),
        "/" => self::compile_bin_op(chunk, file, args, OpCode::OpDiv),
        _ => todo!("callable"),
    }
}

// TODO: rather functional chunk creation
fn compile_bin_op(chunk: &mut ChunkData, file: &FileLex, args: &[Sx], op: OpCode) -> Result<()> {
    for sx in args {
        self::compile_sx(chunk, file, sx)?;
    }

    for _ in 0..args.len() - 1 {
        chunk.push_code(op);
    }

    Ok(())
}

fn compile_sx(chunk: &mut ChunkData, file: &FileLex, sx: &Sx) -> Result<()> {
    match sx {
        // TODO: return location
        // TODO: enter recovery mode
        Sx::Atom(atom) => match atom {
            Atom::Lit(lit) => {
                let tk = &lit.tsp.slice(&file.tks)[0];
                let lexeme = tk.sp.slice(&file.src);
                let value: Value = lexeme.parse().unwrap();
                chunk.push_const(value);
                let ix = chunk.consts().len() as u8 - 1;
                chunk.push_ix_u8(ix);
            }
            Atom::Symbol(symbol) => match symbol.body {
                SymbolBody::Ident => todo!("variable"),
            },
        },
        Sx::List(list) => {
            self::compile_list(chunk, file, list)?;
        }
    }

    Ok(())
}
