//! Bytecode

/// Instruction to the stack-based virtual machine
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum OpCode {
    OpReturn,

    /// Operand: byte index
    OpConst8,
    /// Operand: two bytes index
    OpConst16,

    // OpLoadGlobal,
    // OpSetGlobal,

    // OpLoadLocal,
    // OpSetLocal,

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
    codes: Vec<u8>,
    /// Constant table
    consts: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            codes: Vec::new(),
            consts: Vec::new(),
        }
    }
}

/// Reader
impl Chunk {
    #[inline(always)]
    pub fn code(&self) -> &[u8] {
        &self.codes
    }

    #[inline(always)]
    pub fn read_opcode(&self, ix: usize) -> OpCode {
        unsafe { std::mem::transmute(self.codes[ix]) }
    }

    #[inline(always)]
    pub fn read_u8(&self, ix: usize) -> u8 {
        self.codes[ix]
    }

    #[inline(always)]
    pub fn read_u16(&self, ix: usize) -> u16 {
        ((self.codes[ix] as u16) << 8) | (self.codes[ix + 1] as u16)
    }
}

/// Writer
impl Chunk {
    #[inline(always)]
    pub fn consts(&mut self) -> &Vec<Value> {
        &self.consts
    }

    #[inline(always)]
    pub fn push_const(&mut self, value: Value) -> usize {
        self.consts.push(value);
        self.consts.len() - 1
    }

    #[inline(always)]
    pub fn push_code(&mut self, code: OpCode) {
        self.codes.push(code as u8);
    }

    /// Push 1 byte index that refers to a constant
    #[inline(always)]
    pub fn push_ix_u8(&mut self, x: u8) {
        self.codes.push(OpCode::OpConst8 as u8);
        self.codes.push(x);
    }

    /// Push 2 byte index that refers to a constant
    #[inline(always)]
    pub fn push_ix_u16(&mut self, x: u16) {
        self.codes.push(OpCode::OpConst16 as u8);
        // higher 8 bits
        self.codes.push((x >> 8) as u8);
        // lower 8 bits
        self.codes.push(x as u8);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{code::OpCode::*, Result, Vm};

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

// /// Read chunk as [`OpCode`] s
// pub struct ChunkIter<'a> {
//     chunk: &'a Chunk,
//     i: usize,
// }
//
// impl<'a> Iterator for ChunkIter<'a> {
//     type Item = OpCode;
//     fn next(&mut self) -> Option<Self::Item> {
//         let byte = self.bytes.get(self.i)?;
//         todo!()
//     }
// }

// pub enum OpCodeB { }
