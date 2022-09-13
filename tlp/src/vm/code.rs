//! Bytecode

// TODO: consider endians of opcodes?

use std::fmt::{self, Write};

use crate::vm::{Unit, UnitVariant};

/// Operational code, instruction to the stack-based virtual machine
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
#[repr(u8)]
pub enum Op {
    Ret,

    /// Push `true` to the stack
    PushTrue,
    /// Push `false` to the stack
    PushFalse,
    /// Push `<none>` to the stack
    PushNone,
    /// Pop and do nothing
    Discard,

    /// Operand: byte index
    PushConst8,
    /// Operand: two bytes index
    PushConst16,

    /// Function frame
    AllocFrame8,
    AllocFrame16,

    // locals
    PushLocalUnit8,
    SetLocalUnit8,

    /// Jumps
    Jump16,
    /// Pops a value and jumps if it's true
    JumpIf16,
    /// Pops a value and jumps if it's false
    JumpIfNot16,

    // `f32` arithmetic operations
    NegF32,
    AddF32,
    SubF32,
    MulF32,
    DivF32,

    // `i32` arithmetic operations
    NegI32,
    AddI32,
    SubI32,
    MulI32,
    DivI32,
}

impl Op {
    pub fn operands(&self) -> OpCodeOperands {
        match self {
            Op::PushConst8 | Op::AllocFrame8 | Op::PushLocalUnit8 | Op::SetLocalUnit8 => {
                OpCodeOperands::One
            }
            Op::PushConst16 | Op::AllocFrame16 | Op::Jump16 | Op::JumpIf16 | Op::JumpIfNot16 => {
                OpCodeOperands::Two
            }
            _ => OpCodeOperands::Zero,
        }
    }

    // pub fn n_pops(&self) -> usize

    pub fn as_str(&self) -> &'static str {
        match self {
            Op::Ret => "ret",
            Op::Discard => "discard",
            Op::PushTrue => "push-true",
            Op::PushFalse => "push-false",
            Op::PushNone => "push-none",
            Op::PushConst8 => "push-const-8",
            Op::PushConst16 => "push-const-16",
            Op::AllocFrame8 => "alloc-frame-8",
            Op::AllocFrame16 => "alloc-frame-16",
            Op::PushLocalUnit8 => "push-local-8",
            Op::SetLocalUnit8 => "set-local-8",
            Op::Jump16 => "jump-16",
            Op::JumpIf16 => "jump-if-16",
            Op::JumpIfNot16 => "jump-if-not-16",
            Op::NegF32 => "neg-f32",
            Op::AddF32 => "add-f32",
            Op::SubF32 => "sub-f32",
            Op::MulF32 => "mul-f32",
            Op::DivF32 => "div-f32",
            Op::NegI32 => "neg-i32",
            Op::AddI32 => "add-i32",
            Op::SubI32 => "sub-i32",
            Op::MulI32 => "mul-i32",
            Op::DivI32 => "div-i32",
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum OpCodeOperands {
    Zero,
    One,
    Two,
}

impl Into<u8> for Op {
    fn into(self) -> u8 {
        self as u8
    }
}

/// Compile-time representation of a value of any type
#[derive(Debug, Clone)]
pub enum TypedLiteral {
    F32(f32),
    I32(i32),
    Bool(bool),
}

impl TypedLiteral {
    pub fn into_unit(&self) -> Unit {
        match self {
            Self::F32(x) => x.into_unit(),
            Self::I32(x) => x.into_unit(),
            Self::Bool(x) => x.into_unit(),
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
    pub fn bytes(&self) -> &[u8] {
        &self.codes
    }

    #[inline(always)]
    pub fn read_opcode(&self, ix: usize) -> Op {
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

    fn write_u16(&mut self, data: u16) {
        // higher 8 bits
        self.codes.push((data >> 8) as u8);
        // lower 8 bits
        self.codes.push(data as u8);
    }
}

/// Code writer
impl Chunk {
    #[inline(always)]
    pub fn write_code(&mut self, code: Op) {
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
        self.codes.push(Op::PushConst8 as u8);
        self.codes.push(idx);
    }

    /// Pushes a literal index of two bytes
    #[inline(always)]
    pub fn write_idx_raw_u16(&mut self, idx: u16) {
        self.codes.push(Op::PushConst16 as u8);
        self.write_u16(idx);
    }

    // --------------------------------------------------------------------------------
    // Call frame
    // --------------------------------------------------------------------------------

    #[inline(always)]
    pub fn write_alloc_locals_u8(&mut self, idx: u8) {
        self.codes.push(Op::AllocFrame8 as u8);
        self.codes.push(idx);
    }

    #[inline(always)]
    pub fn write_alloc_locals_u16(&mut self, idx: u16) {
        self.codes.push(Op::AllocFrame16 as u8);
        self.write_u16(idx);
    }

    // --------------------------------------------------------------------------------
    // Locals
    // --------------------------------------------------------------------------------

    #[inline(always)]
    pub fn write_push_local_u8(&mut self, idx: u8) {
        self.codes.push(Op::PushLocalUnit8 as u8);
        self.codes.push(idx);
    }

    #[inline(always)]
    pub fn write_set_local_u8(&mut self, idx: u8) {
        self.codes.push(Op::SetLocalUnit8 as u8);
        self.codes.push(idx);
    }
}

/// Instruction pointer for jumping
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ip(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JumpAnchor(u16);

impl JumpAnchor {
    fn new(chunk: &Chunk) -> Self {
        JumpAnchor(chunk.codes.len() as u16)
    }

    pub fn write_ip(&self, chunk: &mut Chunk) {
        let ip = chunk.ip();
        self.write_ip_at(chunk, ip);
    }

    pub fn write_ip_at(&self, chunk: &mut Chunk, ip: Ip) {
        assert!(ip.0 <= u16::MAX as usize);
        let ip = ip.0 as u16;

        chunk.codes[self.0 as usize] = (ip >> 8) as u8;
        chunk.codes[self.0 as usize + 1] = ip as u8;
    }
}

impl Chunk {
    // --------------------------------------------------------------------------------
    // Jumps
    // --------------------------------------------------------------------------------

    /// Returns current instruction pointer
    #[inline(always)]
    pub fn ip(&self) -> Ip {
        Ip(self.codes.len())
    }

    /// Returns [`JumpAnchor`] for overwriting jump target later
    #[inline(always)]
    pub fn write_jump_u16(&mut self) -> JumpAnchor {
        self.codes.push(Op::Jump16 as u8);
        let anchor = JumpAnchor::new(self);
        self.write_u16(0);
        anchor
    }

    /// Returns [`JumpAnchor`] for overwriting jump target later
    #[inline(always)]
    pub fn write_jump_if_u16(&mut self) -> JumpAnchor {
        self.codes.push(Op::JumpIf16 as u8);
        let anchor = JumpAnchor::new(self);
        self.write_u16(0);
        anchor
    }

    /// Returns [`JumpAnchor`] for overwriting jump target later
    #[inline(always)]
    pub fn write_jump_if_not_u16(&mut self) -> JumpAnchor {
        self.codes.push(Op::JumpIfNot16 as u8);
        let anchor = JumpAnchor::new(self);
        self.write_u16(0);
        anchor
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

impl Chunk {
    pub fn disassemble(&self) -> Result<String, fmt::Error> {
        let mut s = String::new();
        self.disassemble_into(&mut s)?;
        Ok(s)
    }

    pub fn disassemble_into(&self, s: &mut String) -> fmt::Result {
        let mut bytes = self.codes.iter().enumerate();

        loop {
            let (ip, b) = match bytes.next() {
                Some((ip, b)) => (ip, *b),
                None => break,
            };

            // FIXME: implement `TryFrom`, maybe using `num_enum`
            let op: Op = unsafe { std::mem::transmute(b) };
            Self::write_opcode(ip, op, &mut bytes, s)?;
        }

        Ok(())
    }

    fn write_opcode<'a>(
        ip: usize,
        op: Op,
        bytes: &mut impl Iterator<Item = (usize, &'a u8)>,
        s: &mut String,
    ) -> fmt::Result {
        fn next<'a>(bytes: &mut impl Iterator<Item = (usize, &'a u8)>) -> TransparentOptionU8 {
            if let Some((_, b)) = bytes.next() {
                TransparentOptionU8(Some(*b))
            } else {
                TransparentOptionU8(None)
            }
        }

        struct TransparentOptionU8(Option<u8>);

        impl fmt::Display for TransparentOptionU8 {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if let Some(x) = self.0 {
                    fmt::Display::fmt(&x, f)
                } else {
                    fmt::Display::fmt("<none>", f)
                }
            }
        }

        match op.operands() {
            OpCodeOperands::Zero => writeln!(s, "{:3}: {}", ip, op.as_str()),
            OpCodeOperands::One => {
                // TODO: unwrap
                writeln!(s, "{:3}: {:15} {}", ip, op.as_str(), next(bytes))
            }
            OpCodeOperands::Two => {
                // TODO: unwrap
                writeln!(
                    s,
                    "{:3}: {:15} {} {}",
                    ip,
                    op.as_str(),
                    next(bytes),
                    next(bytes)
                )?;
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::{code::Op::*, Result, Vm};

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
            chunk.write_code(SubF32); // -

            chunk.write_idx_raw_u16(2); // 16.0
            chunk.write_code(DivF32); // /

            chunk.write_code(NegF32); // -

            chunk.write_code(Ret);

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
