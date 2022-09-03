//! Jars

use base::jar::InputFile;

use crate::ir::{item::Item, IrJar};

/// File parsed into items
#[salsa::tracked(jar = IrJar)]
pub struct ParsedFile {
    #[id]
    pub input_file: InputFile,

    /// The items found in the file.
    #[return_ref]
    pub items: Vec<Item>,
}
