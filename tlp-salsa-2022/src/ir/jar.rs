//! Jars

use crate::{
    base::jar::InputFile,
    ir::{self, item::Item},
};

#[salsa::tracked(jar = ir::IrJar)]
pub struct ParsedFile {
    #[id]
    input_file: InputFile,

    /// The items found in the file.
    #[return_ref]
    items: Vec<Item>,
}
