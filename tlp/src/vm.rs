//! Bytecode virtual machine (stack-based)

pub mod code;
pub mod stack;

use std::ops;

use thiserror::Error;

use self::{code::*, stack::Stack};

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
    MissingIndex { code: Op, at: usize },
    #[error("nothing to negate")]
    NothingToNegate,
    #[error("end of bytecode while applying operator {op:?}")]
    EobWhileOp { op: Op },
}

/// Toy Lisp bytecode virtual machine
#[derive(Debug)]
pub struct Vm {
    /// Chunk of bytecode instructions and constants
    chunk: Chunk,
    /// Instuction pointer, index to the bytecode chunk
    ip: usize,
    stack: Stack,
}

impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    pub fn ip(&self) -> usize {
        self.ip
    }

    /// Stack bytes
    pub fn units(&self) -> &[Unit] {
        self.stack.units()
    }

    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    fn bump_u8(&mut self) -> u8 {
        let b = self.chunk.read_u8(self.ip);
        self.ip += 1;
        b
    }

    fn bump_u16(&mut self) -> u16 {
        let b = self.chunk.read_u16(self.ip);
        self.ip += 2;
        b
    }

    fn bump_opcode(&mut self) -> Op {
        let code: Op = self.chunk.read_opcode(self.ip);
        self.ip += 1;
        code
    }
}

/// Run
impl Vm {
    pub fn run(&mut self) -> Result<()> {
        let chunk_len = self.chunk.bytes().len();

        while self.ip < chunk_len {
            let code = self.bump_opcode();

            match code {
                Op::Ret => {
                    // REMARK: the return value has to be poped by the caller (bad design?)
                    return Ok(());
                }

                Op::PushTrue => {
                    self.stack.push(true.into_unit());
                }
                Op::PushFalse => {
                    self.stack.push(false.into_unit());
                }
                Op::PushNone => {
                    self.stack.push(Default::default());
                }
                Op::Discard => {
                    self.stack.pop().unwrap();
                }

                // constants
                Op::PushConst8 => {
                    let const_ix = self.bump_u8();
                    let unit = self.chunk.read_literal_u8(const_ix);
                    self.stack.push(unit);
                }

                Op::PushConst16 => {
                    let const_ix = self.bump_u16();
                    let unit = self.chunk.read_literal_u16(const_ix);
                    self.stack.push(unit);
                }

                // call frame
                Op::AllocFrame8 => {
                    let local_capacity = self.bump_u8();
                    let local_capacity = local_capacity as usize;
                    self.stack.push_call_frame(local_capacity);
                }
                Op::AllocFrame16 => {
                    let local_capacity = self.bump_u16();
                    let local_capacity = local_capacity as usize;
                    self.stack.push_call_frame(local_capacity);
                }

                // locals
                Op::PushLocalUnit8 => {
                    let local_ix = self.bump_u8();
                    let local = self.stack.read_local_u8(local_ix);
                    self.stack.push(local);
                }

                Op::SetLocalUnit8 => {
                    let local_ix = self.bump_u8();
                    let unit = self.stack.pop().unwrap();
                    self.stack.set_local_u8(local_ix, unit);
                }

                // jump
                Op::Jump16 => {
                    let ip = self.bump_u16();
                    self.ip = ip as usize;
                }
                Op::JumpIf16 => {
                    let ip = self.bump_u16();
                    let b = bool::from_unit(self.stack.pop().unwrap());
                    if b {
                        self.ip = ip as usize;
                    }
                }
                Op::JumpIfNot16 => {
                    let ip = self.bump_u16();
                    let b = bool::from_unit(self.stack.pop().unwrap());
                    if !b {
                        self.ip = ip as usize;
                    }
                }

                // `f32` operators (builtin)
                Op::NegF32 => {
                    self.unit_op(|x: f32| -x)
                        .map_err(|_| VmError::NothingToNegate)?;
                }
                Op::AddF32 => {
                    self.binary_op::<f32>(Op::AddF32, ops::Add::<f32>::add)?;
                }
                Op::SubF32 => {
                    self.binary_op::<f32>(Op::SubF32, ops::Sub::<f32>::sub)?;
                }
                Op::MulF32 => {
                    self.binary_op::<f32>(Op::MulF32, ops::Mul::<f32>::mul)?;
                }
                Op::DivF32 => {
                    self.binary_op::<f32>(Op::DivF32, ops::Div::<f32>::div)?;
                }

                // `i32` operators (builtin)
                Op::NegI32 => {
                    self.unit_op(|x: i32| -x)
                        .map_err(|_| VmError::NothingToNegate)?;
                }
                Op::AddI32 => {
                    self.binary_op::<i32>(Op::AddI32, ops::Add::<i32>::add)?;
                }
                Op::SubI32 => {
                    self.binary_op::<i32>(Op::SubI32, ops::Sub::<i32>::sub)?;
                }
                Op::MulI32 => {
                    self.binary_op::<i32>(Op::MulI32, ops::Mul::<i32>::mul)?;
                }
                Op::DivI32 => {
                    self.binary_op::<i32>(Op::DivI32, ops::Div::<i32>::div)?;
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
    fn binary_op<T: UnitVariant>(&mut self, op: Op, oper: impl Fn(T, T) -> T) -> Result<()> {
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

fn debug_assert_zeros(unit: Unit, n: usize) {
    for i in n..8 {
        debug_assert_eq!(unit[i], 0)
    }
}

impl UnitVariant for f32 {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 4);
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        f32::from_be_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl UnitVariant for u32 {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 4);
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        u32::from_be_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl UnitVariant for i32 {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 4);
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        i32::from_be_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl UnitVariant for bool {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 1);
        let x = u8::from_be(unit[0]);
        match x {
            0 => false,
            1 => true,
            _ => unreachable!("bool with wrong encoding: {}", x),
        }
    }

    fn into_unit(self) -> Unit {
        let byte = (self as u8).to_be();
        [byte, 0, 0, 0, 0, 0, 0, 0]
    }
}
