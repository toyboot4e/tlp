//! Bytecode

/// Instruction to the stack-based virtual machine
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum OpCode {
    OpReturn,

    /// Operand: byte index
    OpConst8,
    /// Operand: two bytes index
    OpConst16,

    // arithmetic
    OpNegate,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

/// Constant value FIXME: use `enum`?
pub type Value = f64;

// TODO: disassemble

/// Bytecode with static context
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    /// Bytecode ([`OpCode`] and operands of them)
    bytes: Vec<u8>,
    /// Constant table
    consts: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            consts: Vec::new(),
        }
    }
}

/// Reader
impl Chunk {
    #[inline(always)]
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    #[inline(always)]
    pub fn read_opcode(&self, ix: usize) -> OpCode {
        unsafe { std::mem::transmute(self.bytes[ix]) }
    }

    #[inline(always)]
    pub fn read_u8(&self, ix: usize) -> u8 {
        self.bytes[ix]
    }

    #[inline(always)]
    pub fn read_u16(&self, ix: usize) -> u16 {
        ((self.bytes[ix] as u16) << 8) | (self.bytes[ix + 1] as u16)
    }
}

/// Writer
impl Chunk {
    #[inline(always)]
    pub fn consts(&mut self) -> &Vec<Value> {
        &self.consts
    }

    #[inline(always)]
    pub fn push_const(&mut self, value: Value) {
        self.consts.push(value)
    }

    #[inline(always)]
    pub fn push_code(&mut self, code: OpCode) {
        self.bytes.push(code as u8);
    }

    /// Push constant that is referred to by one-byte index
    #[inline(always)]
    pub fn push_ix_u8(&mut self, x: u8) {
        self.bytes.push(OpCode::OpConst8 as u8);
        self.bytes.push(x);
    }

    /// Push constant that is referred to by two-byte index
    #[inline(always)]
    pub fn push_ix_u16(&mut self, x: u16) {
        self.bytes.push(OpCode::OpConst16 as u8);
        // higher 8 bits
        self.bytes.push((x >> 8) as u8);
        // lower 8 bits
        self.bytes.push(x as u8);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{Vm, Result, code::OpCode::*};

    /// Tests `-((64.0 - 32.0) / 16.0)` results in `2.0`
    #[test]
    fn arithmetic() -> Result<()> {
        let chunk = {
            let mut chunk = Chunk::default();

            // NOTE: use 2^x considering the accuracy of floating values
            chunk.push_const(64.0);
            chunk.push_const(32.0);
            chunk.push_const(16.0);

            chunk.push_ix_u8(0); // 64.0
            chunk.push_ix_u8(1); // 32.0
            chunk.push_code(OpSub); // -

            chunk.push_ix_u16(2); // 16.0
            chunk.push_code(OpDiv); // /

            chunk.push_code(OpNegate); // -

            chunk.push_code(OpReturn);

            chunk
        };

        let mut vm = Vm::new(chunk);

        vm.run()?;
        assert_eq!(Some(&-2.0), vm.stack().last());

        Ok(())
    }
}
