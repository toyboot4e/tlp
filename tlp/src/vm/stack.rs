//! Stack

use crate::vm::Word;

/// Stack of [`Word`] s
#[derive(Debug, Clone)]
pub struct Stack {
    words: Vec<Word>,
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
            words: Vec::new(),
            frames: vec![root_frame],
        }
    }

    pub fn words(&self) -> &[Word] {
        &self.words
    }

    /// Returns index to the beginning of temporary variables
    pub fn tmp_offset(&self) -> usize {
        let frame = self.frames.last().unwrap();
        frame.offset + frame.n_args + frame.n_locals
    }

    pub fn push(&mut self, word: Word) {
        self.words.push(word);
    }

    pub fn pop(&mut self) -> Option<Word> {
        debug_run(|| {
            let n = self.words.len();
            if n <= self.tmp_offset() {
                panic!(
                    "invalid pop detected\ntmp offset: {}\ncall frames: {:?}\nstack: {:?}",
                    self.tmp_offset(),
                    self.frames,
                    self.words
                );
            }
        });

        self.words.pop()
    }

    pub fn peek(&mut self) -> Option<&Word> {
        self.words.last()
    }

    /// Pushes a runtime call frame, but without including the arguments
    pub fn push_call_frame(&mut self, n_locals: usize) {
        let offset = self.words.len();

        for _ in 0..n_locals {
            self.words.push(Word::default());
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

        let new_len = self.words.len() - (frame.n_args + frame.n_locals);

        assert_eq!(
            new_len, frame.offset,
            "wrong stack length on pop.\ncall frame: {:?}\nstack: {:?}",
            frame, self.words,
        );

        self.words.truncate(new_len);
    }

    pub fn set_local_u8(&mut self, index: u8, word: Word) {
        let frame = self.frames.last().unwrap();
        // REMARK:
        let i = frame.offset + frame.n_args + index as usize;
        self.words[i] = word;
    }

    pub fn read_local_u8(&mut self, local_index: u8) -> Word {
        let frame = self.frames.last().unwrap();

        debug_run(|| {
            assert!(
                (local_index as usize) < frame.n_vars(),
                "invalid local index or maybe popped too much: `{}`. {:?}",
                local_index,
                frame,
            );

            assert!(
                frame.offset + frame.n_vars() <= self.words.len(),
                "maybe popped too much"
            );
        });

        let i = frame.offset + local_index as usize;
        self.words[i]
    }
}
