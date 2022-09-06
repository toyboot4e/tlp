//! Stack

use crate::vm::Unit;

/// Stack of [`Unit`] s
#[derive(Debug, Clone)]
pub struct Stack {
    units: Vec<Unit>,
    frames: Vec<CallFrame>,
}

#[derive(Debug, Clone)]
struct CallFrame {
    offset: usize,
    capacity: usize,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            units: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub fn units(&self) -> &[Unit] {
        &self.units
    }

    pub fn push(&mut self, unit: Unit) {
        self.units.push(unit);
    }

    pub fn pop(&mut self) -> Option<Unit> {
        self.units.pop()
    }

    pub fn push_call_frame(&mut self, n_units: usize) {
        let offset = self.units.len();
        let capacity = n_units;

        for _ in 0..n_units {
            self.units.push(Unit::default());
        }

        let frame = CallFrame { offset, capacity };

        self.frames.push(frame);
    }

    pub fn set_local_u8(&mut self, index: u8, unit: Unit) {
        let frame = self.frames.last().unwrap();
        let i = frame.offset + index as usize;
        self.units[i] = unit;
    }

    pub fn read_local_u8(&mut self, index: u8) -> Unit {
        let frame = self.frames.last().unwrap();
        let i = frame.offset + index as usize;
        self.units[i]
    }
}
