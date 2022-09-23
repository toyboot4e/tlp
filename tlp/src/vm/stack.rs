//! Stack

use crate::vm::Unit;

/// Stack of [`Unit`] s
#[derive(Debug, Clone)]
pub struct Stack {
    units: Vec<Unit>,
    frames: Vec<CallFrame>,
}

#[derive(Debug, Clone, Default)]
struct CallFrame {
    /// Offset of the first argument
    offset: usize,
    n_args: usize,
    n_locals: usize,
}

impl CallFrame {
    /// The number of arguments + the number of local variables
    pub fn n_vars(&self) -> usize {
        self.n_args + self.n_locals
    }
}

fn debug_run(f: impl FnOnce()) {
    f()
}

impl Stack {
    pub fn new() -> Self {
        // guard for `Self::tmp_offset
        let root_frame = CallFrame::default();

        Self {
            units: Vec::new(),
            frames: vec![root_frame],
        }
    }

    pub fn units(&self) -> &[Unit] {
        &self.units
    }

    /// Returns index to the beginning of temporary variables
    pub fn tmp_offset(&self) -> usize {
        let frame = self.frames.last().unwrap();
        frame.offset + frame.n_args + frame.n_locals
    }

    pub fn push(&mut self, unit: Unit) {
        self.units.push(unit);
    }

    pub fn pop(&mut self) -> Option<Unit> {
        debug_run(|| {
            let n = self.units.len();
            if n <= self.tmp_offset() {
                panic!(
                    "invalid pop detected\ntmp offset: {}\ncall frames: {:?}\nstack: {:?}",
                    self.tmp_offset(),
                    self.frames,
                    self.units
                );
            }
        });

        self.units.pop()
    }

    pub fn peek(&mut self) -> Option<&Unit> {
        self.units.last()
    }

    /// Pushes a runtime call frame, but without including the arguments
    pub fn push_call_frame(&mut self, n_locals: usize) {
        let offset = self.units.len();

        for _ in 0..n_locals {
            self.units.push(Unit::default());
        }

        let frame = CallFrame {
            offset,
            // REMARK: call `shift_call_frame_offset` just after this function call
            n_args: 0,
            n_locals,
        };

        self.frames.push(frame);
    }

    /// Shift the call frame's offset so that it can include arguments
    pub fn shift_call_frame_offset(&mut self, n_args: usize) {
        let frame = self.frames.last_mut().unwrap();
        frame.offset -= n_args;
        frame.n_args = n_args;
    }

    pub fn pop_call_frame(&mut self) {
        let frame = self
            .frames
            .pop()
            .unwrap_or_else(|| panic!("bug: tried to pop call frame but none"));

        let new_len = self.units.len() - (frame.n_args + frame.n_locals);

        assert_eq!(
            new_len, frame.offset,
            "wrong stack length on pop.\ncall frame: {:?}\nstack: {:?}",
            frame, self.units,
        );

        self.units.truncate(new_len);
    }

    pub fn set_local_u8(&mut self, index: u8, unit: Unit) {
        let frame = self.frames.last().unwrap();
        // REMARK:
        let i = frame.offset + frame.n_args + index as usize;
        self.units[i] = unit;
    }

    pub fn read_local_u8(&mut self, local_index: u8) -> Unit {
        let frame = self.frames.last().unwrap();

        debug_run(|| {
            assert!(
                (local_index as usize) < frame.n_vars(),
                "invalid local index or maybe popped too much: `{}`. {:?}",
                local_index,
                frame,
            );

            assert!(
                frame.offset + frame.n_vars() <= self.units.len(),
                "maybe popped too much"
            );
        });

        let i = frame.offset + local_index as usize;
        self.units[i]
    }
}
