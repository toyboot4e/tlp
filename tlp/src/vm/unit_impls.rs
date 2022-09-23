//! [`UnitVariant`] impls
use super::*;

fn debug_assert_zeros(unit: Unit, n: usize) {
    for i in n..::core::mem::size_of::<Unit>() {
        debug_assert_eq!(unit[i], 0)
    }
}

impl UnitVariant for f32 {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 4);
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        f32::from_be_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl UnitVariant for u32 {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 4);
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        u32::from_be_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl UnitVariant for i32 {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 4);
        let bytes: [u8; 4] = unit[0..4].try_into().unwrap();
        i32::from_be_bytes(bytes)
    }

    fn into_unit(self) -> Unit {
        let bytes = self.to_be_bytes();
        [bytes[0], bytes[1], bytes[2], bytes[3], 0, 0, 0, 0]
    }
}

impl UnitVariant for bool {
    fn from_unit(unit: Unit) -> Self {
        debug_assert_zeros(unit, 1);
        let x = u8::from_be(unit[0]);
        match x {
            0 => false,
            1 => true,
            _ => unreachable!("bool with wrong encoding: {}", x),
        }
    }

    fn into_unit(self) -> Unit {
        let byte = (self as u8).to_be();
        [byte, 0, 0, 0, 0, 0, 0, 0]
    }
}
