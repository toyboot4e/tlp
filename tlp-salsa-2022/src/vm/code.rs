//! Bytecode

// TODO: disassemble

use crate::vm::{Unit, UnitVariant};

/// Instruction to the stack-based virtual machine
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum OpCode {
    OpReturn,

    /// Operand: byte index
    OpConst8,
    /// Operand: two bytes index
    OpConst16,

    // local variables (one word only)
    OpAllocLocals8,
    OpPushLocalUnit8,
    OpSetLocalUnit8,

    // arithmetic operations
    OpNegateF32,
    OpAddF32,
    OpSubF32,
    OpMulF32,
    OpDivF32,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

/// Compile-time representation of a value of any type
#[derive(Debug, Clone)]
pub enum TypedLiteral {
    F32(f32),
    I32(i32),
}

impl TypedLiteral {
    pub fn into_unit(&self) -> Unit {
        match self {
            Self::F32(x) => x.into_unit(),
            Self::I32(x) => x.into_unit(),
        }
    }
}

/// Runtime literals
#[derive(Debug, Clone, Default)]
struct LiteralArena {
    units: Vec<Unit>,
}

/// Index to a literal table
#[derive(Debug, Clone)]
pub struct LiteralIndex {
    raw: usize,
}

impl LiteralArena {
    pub fn insert(&mut self, value: TypedLiteral) -> LiteralIndex {
        let idx = LiteralIndex {
            raw: self.units.len(),
        };

        let unit = value.into_unit();
        self.units.push(unit);

        idx
    }

    pub fn read(&self, idx: LiteralIndex) -> Unit {
        self.units[idx.raw]
    }
}

/// Bytecode and constants
#[derive(Debug, Clone, Default)]
pub struct Chunk {
    codes: Vec<u8>,
    literals: LiteralArena,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }
}

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

/// OpCode writer
impl Chunk {
    #[inline(always)]
    pub fn write_code(&mut self, code: OpCode) {
        self.codes.push(code as u8);
    }

    /// Pushes a literal index
    #[inline(always)]
    pub fn write_ix(&mut self, idx: LiteralIndex) {
        if idx.raw < 2 << 8 {
            return self.write_idx_raw_u8(idx.raw as u8);
        }

        if idx.raw < 2 << 16 {
            return self.write_idx_raw_u16(idx.raw as u16);
        }

        panic!("too big literal index: {}", idx.raw)
    }

    /// Pushes a literal index of one byte
    #[inline(always)]
    pub fn write_idx_raw_u8(&mut self, idx: u8) {
        self.codes.push(OpCode::OpConst8 as u8);
        self.codes.push(idx);
    }

    /// Pushes a literal index of two bytes
    #[inline(always)]
    pub fn write_idx_raw_u16(&mut self, idx: u16) {
        self.codes.push(OpCode::OpConst16 as u8);
        // higher 8 bits
        self.codes.push((idx >> 8) as u8);
        // lower 8 bits
        self.codes.push(idx as u8);
    }

    // --------------------------------------------------------------------------------
    // Call frame
    // --------------------------------------------------------------------------------

    #[inline(always)]
    pub fn write_alloc_locals_u8(&mut self, idx: u8) {
        self.codes.push(OpCode::OpAllocLocals8 as u8);
        self.codes.push(idx);
    }

    // --------------------------------------------------------------------------------
    // Locals
    // --------------------------------------------------------------------------------

    #[inline(always)]
    pub fn write_load_local_u8(&mut self, idx: u8) {
        self.codes.push(OpCode::OpPushLocalUnit8 as u8);
        self.codes.push(idx);
    }

    #[inline(always)]
    pub fn write_set_local_u8(&mut self, idx: u8) {
        self.codes.push(OpCode::OpSetLocalUnit8 as u8);
        self.codes.push(idx);
    }
}

impl Chunk {
    #[inline(always)]
    pub fn store_literal(&mut self, value: TypedLiteral) -> LiteralIndex {
        self.literals.insert(value)
    }

    #[inline(always)]
    pub fn read_literal(&mut self, idx: LiteralIndex) -> Unit {
        self.literals.read(idx)
    }

    #[inline(always)]
    pub fn read_literal_u8(&mut self, idx_u8: u8) -> Unit {
        let idx = LiteralIndex { raw: idx_u8.into() };

        self.read_literal(idx)
    }

    #[inline(always)]
    pub fn read_literal_u16(&mut self, idx_u16: u16) -> Unit {
        let idx = LiteralIndex {
            raw: idx_u16.into(),
        };

        self.read_literal(idx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{code::OpCode::*, Result, Vm};

    /// Tests `-((64.0 - 32.0) / 16.0)` equals to `2.0`
    #[test]
    fn arithmetic() -> Result<()> {
        let chunk = {
            let mut chunk = Chunk::default();

            // NOTE: use 2^x considering the accuracy of floating values
            chunk.store_literal(TypedLiteral::F32(64.0));
            chunk.store_literal(TypedLiteral::F32(32.0));
            chunk.store_literal(TypedLiteral::F32(16.0));

            chunk.write_idx_raw_u8(0); // 64.0
            chunk.write_idx_raw_u8(1); // 32.0
            chunk.write_code(OpSubF32); // -

            chunk.write_idx_raw_u16(2); // 16.0
            chunk.write_code(OpDivF32); // /

            chunk.write_code(OpNegateF32); // -

            chunk.write_code(OpReturn);

            chunk
        };

        let mut vm = Vm::new(chunk);
        vm.run()?;

        assert_eq!(
            Some(&TypedLiteral::F32(-2.0).into_unit()),
            vm.units().last()
        );

        Ok(())
    }
}
