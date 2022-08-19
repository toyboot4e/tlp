//! Bytecode virtual machine (stack-based)

pub mod code;
pub mod stack;

use std::ops;

use thiserror::Error;

use crate::vm::code::*;

pub type Result<T, E = VmError> = std::result::Result<T, E>;

/// Stack item
pub type Unit = [u8; UNIT_SIZE];

/// Stack item size
pub const UNIT_SIZE: usize = 8;

#[derive(Debug, Error)]
pub enum VmError {
    #[error("runtime error")]
    RuntimeError,
    #[error("{code:?} must be followed by an index ({at})")]
    MissingIndex { code: OpCode, at: usize },
    #[error("nothing to negate")]
    NothingToNegate,
    #[error("end of bytecode while applying operator {op:?}")]
    EobWhileOp { op: OpCode },
}

/// Toy Lisp bytecode virtual machine
#[derive(Debug)]
pub struct Vm {
    /// Chunk of bytecode instructions and constants
    chunk: Chunk,
    /// Instuction pointer, index to the bytecode chunk
    ip: usize,
    /// Stack of values
    stack: Vec<Unit>,
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

    /// Stack bytes
    pub fn stack(&mut self) -> &[Unit] {
        &self.stack
    }
}

/// Run
impl Vm {
    pub fn run(&mut self) -> Result<()> {
        let chunk_len = self.chunk.code().len();

        while self.ip < chunk_len {
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

                    let unit = self.chunk.read_literal_u8(const_ix);
                    self.stack.push(unit);
                }

                OpConst16 => {
                    let const_ix = self.chunk.read_u16(self.ip);
                    self.ip += 2;

                    let unit = self.chunk.read_literal_u16(const_ix);
                    self.stack.push(unit);
                }

                OpNegateF32 => {
                    self.unit_op(|x: f32| -x)
                        .map_err(|_| VmError::NothingToNegate)?;
                }
                OpAddF32 => {
                    self.binary_op::<f32>(OpAddF32, ops::Add::<f32>::add)?;
                }
                OpSubF32 => {
                    self.binary_op::<f32>(OpSubF32, ops::Sub::<f32>::sub)?;
                }
                OpMulF32 => {
                    self.binary_op::<f32>(OpMulF32, ops::Mul::<f32>::mul)?;
                }
                OpDivF32 => {
                    self.binary_op::<f32>(OpDivF32, ops::Div::<f32>::div)?;
                }
            }
        }

        Ok(())
    }

    /// Pushes binary operator to the stack
    fn unit_op<T: UnitVariant>(&mut self, oper: impl Fn(T) -> T) -> Result<(), ()> {
        let unit = self.stack.pop().ok_or(())?;
        let out_unit = unit_unary_oper::<T>(unit, oper);
        self.stack.push(out_unit);

        Ok(())
    }

    /// Pushes binary operator to the stack
    fn binary_op<T: UnitVariant>(&mut self, op: OpCode, oper: impl Fn(T, T) -> T) -> Result<()> {
        let y = self.stack.pop().ok_or_else(|| VmError::EobWhileOp { op })?;
        let x = self.stack.pop().ok_or_else(|| VmError::EobWhileOp { op })?;

        let out_unit = T::into_unit(oper(T::from_unit(x), T::from_unit(y)));
        self.stack.push(out_unit);

        Ok(())
    }
}

pub fn unit_unary_oper<T: UnitVariant>(x: Unit, f: impl Fn(T) -> T) -> Unit {
    let x = T::from_unit(x);
    let out = f(x);
    T::into_unit(out)
}

pub fn unit_binary_oper<T: UnitVariant>(x: Unit, y: Unit, f: impl Fn(T, T) -> T) -> Unit {
    let x = T::from_unit(x);
    let y = T::from_unit(y);
    let out = f(x, y);
    T::into_unit(out)
}

pub trait UnitVariant {
    fn from_unit(unit: Unit) -> Self;
    fn into_unit(self) -> Unit;
}

impl UnitVariant for f32 {
    fn from_unit(unit: Unit) -> Self {
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        f32::from_ne_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_ne_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}
