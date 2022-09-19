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
    offset: usize,
    n_units: usize,
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
        frame.offset + frame.n_units
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

    pub fn push_call_frame(&mut self, n_units: usize) {
        let offset = self.units.len();
        let n_units = n_units;

        for _ in 0..n_units {
            self.units.push(Unit::default());
        }

        let frame = CallFrame { offset, n_units };

        self.frames.push(frame);
    }

    pub fn set_local_u8(&mut self, index: u8, unit: Unit) {
        let frame = self.frames.last().unwrap();
        let i = frame.offset + index as usize;
        self.units[i] = unit;
    }

    pub fn read_local_u8(&mut self, local_index: u8) -> Unit {
        let frame = self.frames.last().unwrap();

        debug_run(|| {
            assert!(
                (local_index as usize) < frame.n_units,
                "invalid local index or maybe popped too much"
            );

            assert!(
                frame.offset + frame.n_units <= self.units.len(),
                "maybe popped too much"
            );
        });

        let i = frame.offset + local_index as usize;
        self.units[i]
    }
}
