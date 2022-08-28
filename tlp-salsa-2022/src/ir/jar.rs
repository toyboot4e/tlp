//! Jars

use crate::{
    base::jar::InputFile,
    ir::{self, item::Item},
};

/// The result of parsing an input file like `foo.dada`, or the
/// value from the playground. A program is a collection of files.
#[salsa::tracked(jar = ir::Jar)]
pub struct ParsedFile {
    #[id]
    input_file: InputFile,

    /// The items found in the file.
    #[return_ref]
    items: Vec<Item>,
}
