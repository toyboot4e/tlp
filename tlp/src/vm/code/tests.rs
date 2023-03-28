use super::*;
use crate::vm::{self, code::Op::*, Result};

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

    let word = vm::run_proc(vm::VmProc {
        chunk,
        n_args: 0,
        name: "test".to_string(),
    })?;

    assert_eq!(TypedLiteral::F32(-2.0).into_word(), word);

    Ok(())
}
