//! [`WordInstance`] impls
use super::*;

fn debug_assert_zeros(word: Word, n: usize) {
    for i in n..::core::mem::size_of::<Word>() {
        debug_assert_eq!(word[i], 0)
    }
}

impl WordInstance for f32 {
    fn from_word(word: Word) -> Self {
        debug_assert_zeros(word, 4);
        let bytes: [u8; 4] = word[0..4].try_into().unwrap();
        f32::from_be_bytes(bytes)
    }

    fn into_word(self) -> Word {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl WordInstance for u32 {
    fn from_word(word: Word) -> Self {
        debug_assert_zeros(word, 4);
        let bytes: [u8; 4] = word[0..4].try_into().unwrap();
        u32::from_be_bytes(bytes)
    }

    fn into_word(self) -> Word {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl WordInstance for i32 {
    fn from_word(word: Word) -> Self {
        debug_assert_zeros(word, 4);
        let bytes: [u8; 4] = word[0..4].try_into().unwrap();
        i32::from_be_bytes(bytes)
    }

    fn into_word(self) -> Word {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl WordInstance for bool {
    fn from_word(word: Word) -> Self {
        debug_assert_zeros(word, 1);
        let x = u8::from_be(word[0]);
        match x {
            0 => false,
            1 => true,
            _ => unreachable!("bool with wrong encoding: {}", x),
        }
    }

    fn into_word(self) -> Word {
        let byte = (self as u8).to_be();
        [byte, 0, 0, 0, 0, 0, 0, 0]
    }
}
