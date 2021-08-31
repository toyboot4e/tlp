/*!
Import resolution and macro expansion
*/

pub struct DefMap {
    // block_info: Option<BlockInfo>,
// diags: Vec<Diagnostics>,
}

/// For `DefMap`s computed for a block expression, this stores its location in the parent map.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct BlockInfo {
    // /// The `BlockId` this `DefMap` was created from.
// block: BlockId,
// /// The containing module.
// parent: ModuleId,
}

/// Import resolution and macro expansion
pub fn expand() {
    //
}
