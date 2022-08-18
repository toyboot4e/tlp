//! Bytecode virtual machine (stack-based)

pub mod code;
pub mod stack;

use std::ops;
use thiserror::Error;

use crate::vm::code::*;

pub type Result<T, E = VmError> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum VmError {
    #[error("compile error")]
    CompileError,
    #[error("runtime error")]
    RuntimeError,
    #[error("{code:?} must be followed by an index ({at})")]
    MissingIndex { code: OpCode, at: usize },
    #[error("nothing to negate")]
    NothingToNegate,
    #[error("end of bytecode while applying operator {op:?}")]
    EofWhileOp { op: OpCode },
}

/// Toy Lisp bytecode virtual machine
#[derive(Debug)]
pub struct Vm {
    /// Chunk of bytecode instructions
    chunk: Chunk,
    /// Instuction pointer, index of current bytecode
    ip: usize,
    /// The `Vm` is stack-based
    stack: Vec<Value>,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(256),
        }
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    pub fn ip(&self) -> usize {
        self.ip
    }

    pub fn stack(&mut self) -> &Vec<Value> {
        &self.stack
    }
}

/// Run
impl Vm {
    pub fn run(&mut self) -> Result<()> {
        let chunk_len = self.chunk.code().len();

        while self.ip < chunk_len {
            // FIXME: use bytes
            // consume the next instruction
            let code: OpCode = self.chunk.read_opcode(self.ip);
            self.ip += 1;

            use OpCode::*;
            match code {
                OpReturn => {
                    // REMARK: the return value has to be poped by the caller (bad design?)
                    return Ok(());
                }

                OpConst8 => {
                    let const_ix = self.chunk.read_u8(self.ip);
                    self.ip += 1;

                    let value = self
                        .chunk
                        .consts()
                        .get(const_ix as usize)
                        .cloned()
                        .ok_or_else(|| VmError::MissingIndex {
                            code: OpConst8,
                            at: self.ip,
                        })?;

                    self.stack.push(value);
                }

                OpConst16 => {
                    let const_ix = self.chunk.read_u16(self.ip);
                    self.ip += 2;

                    let value = self
                        .chunk
                        .consts()
                        .get(const_ix as usize)
                        .cloned()
                        .ok_or_else(|| VmError::MissingIndex {
                            code: OpConst16,
                            at: self.ip,
                        })?;

                    self.stack.push(value);
                    // println!("{}, {} => {:?}", "byte2", ix, value);
                }

                OpNegate => {
                    let value = -self.stack.pop().ok_or_else(|| VmError::NothingToNegate)?;
                    self.stack.push(value);
                }

                OpAdd => {
                    self.binary_op(OpAdd, ops::Add::add)?;
                }
                OpSub => {
                    self.binary_op(OpSub, ops::Sub::sub)?;
                }
                OpMul => {
                    self.binary_op(OpMul, ops::Mul::mul)?;
                }
                OpDiv => {
                    self.binary_op(OpDiv, ops::Div::div)?;
                }
            }
        }

        Ok(())
    }

    /// Pushes binary operator to the stack
    fn binary_op(&mut self, op: OpCode, apply_op: impl Fn(Value, Value) -> Value) -> Result<()> {
        // bbbb aaaa
        let b = self.stack.pop().ok_or_else(|| VmError::EofWhileOp { op })?;

        let a = self.stack.pop().ok_or_else(|| VmError::EofWhileOp { op })?;

        self.stack.push(apply_op(a, b));

        Ok(())
    }
}
