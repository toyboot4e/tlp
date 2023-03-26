//! Bytecode virtual machine (stack-based)

pub mod code;
pub mod stack;

mod unit_impls;

use std::ops;

use thiserror::Error;
use typed_index_collections::{TiSlice, TiVec};

use self::{code::*, stack::Stack};

pub type Result<T, E = VmError> = std::result::Result<T, E>;

/// Stack item
pub type Unit = [u8; UNIT_SIZE];

/// Stack item size
pub const UNIT_SIZE: usize = 8;

/// [`Vm`] stack data unit
pub trait UnitVariant {
    fn from_unit(unit: Unit) -> Self;
    fn into_unit(self) -> Unit;
}

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

/// Call frame state for the VM (remembers the instruction pointer)
#[derive(Debug)]
struct VmCallFrame {
    ip: usize,
}

/// ID of [`VmProc`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VmProcId(pub usize);

impl From<usize> for VmProcId {
    fn from(x: usize) -> Self {
        Self(x)
    }
}

impl From<VmProcId> for usize {
    fn from(id: VmProcId) -> usize {
        id.0
    }
}

/// Runtime representation of a procedure
#[derive(Debug)]
pub struct VmProc {
    /// Chunk of bytecode instructions and constants
    pub chunk: Chunk,
    pub n_args: usize,
    /// Metadata
    pub name: String,
}

/// Toy Lisp bytecode virtual machine
#[derive(Debug)]
pub struct Vm {
    procs: TiVec<VmProcId, VmProc>,
    frames: Vec<VmCallFrame>,
    stack: Stack,
}

pub fn run_proc(vm_proc: VmProc) -> Result<Unit> {
    let mut procs = TiVec::new();
    let vm_proc_index = procs.push_and_get_key(vm_proc);
    let mut vm = Vm::new(procs);
    vm.run_proc(vm_proc_index)
}

impl Vm {
    pub fn new(procs: TiVec<VmProcId, VmProc>) -> Self {
        Self {
            procs,
            frames: Vec::new(),
            stack: Stack::new(),
        }
    }

    pub fn run_proc(&mut self, proc: VmProcId) -> Result<Unit> {
        let res = self.bind(proc).run();

        debug_assert!(self.frames.is_empty(), "any call frame after run?");
        debug_assert!(
            self.stack.units().is_empty(),
            "any value on stack after run?:\n{:?}",
            self.stack.units()
        );

        res
    }

    fn bind(&mut self, proc_id: VmProcId) -> VmBind<'_> {
        VmBind {
            proc_id,
            procs: &self.procs,
            frames: &mut self.frames,
            stack: &mut self.stack,
        }
    }

    pub fn proc(&self, proc: VmProcId) -> &VmProc {
        &self.procs[proc]
    }

    pub fn procs(&self) -> &[VmProc] {
        &self.procs.raw
    }
}

/// Stack
impl Vm {
    pub fn stack(&self) -> &Stack {
        &self.stack
    }

    /// Stack bytes
    pub fn units(&self) -> &[Unit] {
        self.stack.units()
    }
}

/// [`Vm`] binded to a particular procedure
#[derive(Debug)]
struct VmBind<'a> {
    proc_id: VmProcId,
    procs: &'a TiSlice<VmProcId, VmProc>,
    frames: &'a mut Vec<VmCallFrame>,
    stack: &'a mut Stack,
}

/// Operation codes
// TODO(pref): more efficient access?
impl<'a> VmBind<'a> {
    pub fn ip(&self) -> usize {
        self.frames.last().unwrap().ip
    }

    fn set_ip(&mut self, ip: usize) {
        self.frames.last_mut().unwrap().ip = ip;
    }

    fn inc_ip(&mut self) {
        self.frames.last_mut().unwrap().ip += 1;
    }

    fn chunk(&self) -> &Chunk {
        &self.procs[self.proc_id].chunk
    }
}

/// Stack
impl<'a> VmBind<'a> {
    fn bump_u8(&mut self) -> u8 {
        let b = self.chunk().read_u8(self.ip());
        self.inc_ip();
        b
    }

    fn bump_u16(&mut self) -> u16 {
        let b = self.chunk().read_u16(self.ip());
        self.inc_ip();
        self.inc_ip();
        b
    }

    fn bump_opcode(&mut self) -> Op {
        let code: Op = self.chunk().read_opcode(self.ip());
        self.inc_ip();
        code
    }
}

/// Run
impl<'a> VmBind<'a> {
    fn run(&mut self) -> Result<Unit> {
        // use the binded procedure's call frame
        self.frames.push(VmCallFrame { ip: 0 });

        let chunk_len = self.chunk().bytes().len();

        while self.ip() < chunk_len {
            let code = self.bump_opcode();

            match code {
                Op::Ret => {
                    self.frames.pop();
                    let unit = self.stack.pop().unwrap();
                    self.stack.pop_call_frame();
                    return Ok(unit);
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
                    let unit = self.chunk().read_literal_u8(const_ix);
                    self.stack.push(unit);
                }

                Op::PushConst16 => {
                    let const_ix = self.bump_u16();
                    let unit = self.chunk().read_literal_u16(const_ix);
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
                Op::ShiftBack8 => {
                    let shift = self.bump_u8();
                    self.stack.shift_call_frame_offset(shift as usize);
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
                    self.set_ip(ip as usize);
                }
                Op::JumpIf16 => {
                    let ip = self.bump_u16();
                    let b = bool::from_unit(self.stack.pop().unwrap());
                    if b {
                        self.set_ip(ip as usize);
                    }
                }
                Op::JumpIfNot16 => {
                    let ip = self.bump_u16();
                    let b = bool::from_unit(self.stack.pop().unwrap());
                    if !b {
                        self.set_ip(ip as usize);
                    }
                }

                // call
                Op::CallProc16 => {
                    let proc_id = self.bump_u16();
                    let proc_id = VmProcId(proc_id as usize);

                    // TODO: re-consider if we separate call frame operation here
                    // self.frames.push(VmCallFrame { ip: 0, proc_id });

                    let unit = VmBind {
                        procs: self.procs,
                        proc_id,
                        frames: self.frames,
                        stack: self.stack,
                    }
                    .run()?;
                    self.stack.push(unit);
                }

                // `f32` operators (builtin)
                Op::NegF32 => {
                    self.unit_op(|x: f32| -x)
                        .map_err(|_| VmError::NothingToNegate)?;
                }
                op @ Op::AddF32 => {
                    self.binary_op::<f32>(op, ops::Add::<f32>::add)?;
                }
                op @ Op::SubF32 => {
                    self.binary_op::<f32>(op, ops::Sub::<f32>::sub)?;
                }
                op @ Op::MulF32 => {
                    self.binary_op::<f32>(op, ops::Mul::<f32>::mul)?;
                }
                op @ Op::DivF32 => {
                    self.binary_op::<f32>(op, ops::Div::<f32>::div)?;
                }

                // `i32` operators (builtin)
                Op::NegI32 => {
                    self.unit_op(|x: i32| -x)
                        .map_err(|_| VmError::NothingToNegate)?;
                }
                op @ Op::AddI32 => {
                    self.binary_op::<i32>(op, ops::Add::<i32>::add)?;
                }
                op @ Op::SubI32 => {
                    self.binary_op::<i32>(op, ops::Sub::<i32>::sub)?;
                }
                op @ Op::MulI32 => {
                    self.binary_op::<i32>(op, ops::Mul::<i32>::mul)?;
                }
                op @ Op::DivI32 => {
                    self.binary_op::<i32>(op, ops::Div::<i32>::div)?;
                }

                // comparison
                op @ Op::EqBool => {
                    self.binary_op_2::<bool, bool>(op, |x1, x2| x1 == x2)?;
                }
                op @ Op::EqI32 => {
                    self.binary_op_2::<i32, bool>(op, |x1, x2| x1 == x2)?;
                }
                op @ Op::EqF32 => {
                    self.binary_op_2::<f32, bool>(op, |x1, x2| x1 == x2)?;
                }

                op @ Op::NotEqBool => {
                    self.binary_op_2::<bool, bool>(op, |x1, x2| x1 != x2)?;
                }
                op @ Op::NotEqI32 => {
                    self.binary_op_2::<i32, bool>(op, |x1, x2| x1 != x2)?;
                }
                op @ Op::NotEqF32 => {
                    self.binary_op_2::<f32, bool>(op, |x1, x2| x1 != x2)?;
                }

                op @ Op::LtI32 => {
                    self.binary_op_2::<i32, bool>(op, |x1, x2| x1 < x2)?;
                }
                op @ Op::LtF32 => {
                    self.binary_op_2::<f32, bool>(op, |x1, x2| x1 < x2)?;
                }

                op @ Op::LeI32 => {
                    self.binary_op_2::<i32, bool>(op, |x1, x2| x1 <= x2)?;
                }
                op @ Op::LeF32 => {
                    self.binary_op_2::<f32, bool>(op, |x1, x2| x1 <= x2)?;
                }

                op @ Op::GtI32 => {
                    self.binary_op_2::<i32, bool>(op, |x1, x2| x1 > x2)?;
                }
                op @ Op::GtF32 => {
                    self.binary_op_2::<f32, bool>(op, |x1, x2| x1 > x2)?;
                }

                op @ Op::GeI32 => {
                    self.binary_op_2::<i32, bool>(op, |x1, x2| x1 >= x2)?;
                }
                op @ Op::GeF32 => {
                    self.binary_op_2::<f32, bool>(op, |x1, x2| x1 >= x2)?;
                }
            }
        }

        unreachable!("no return code");
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
        self.binary_op_2::<T, T>(op, oper)
    }

    /// Pushes binary operator to the stack
    fn binary_op_2<T: UnitVariant, Res: UnitVariant>(
        &mut self,
        op: Op,
        oper: impl Fn(T, T) -> Res,
    ) -> Result<()> {
        let y = self.stack.pop().ok_or_else(|| VmError::EobWhileOp { op })?;
        let x = self.stack.pop().ok_or_else(|| VmError::EobWhileOp { op })?;

        let out_unit = Res::into_unit(oper(T::from_unit(x), T::from_unit(y)));
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
