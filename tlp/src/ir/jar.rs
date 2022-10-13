//! Jars

use base::jar::InputFile;

use crate::{
    ir::{item::Item, IrJar},
    syntax::cst::ParseError,
};

/// File parsed into items
#[salsa::tracked(jar = IrJar)]
pub struct ParsedFile {
    #[id]
    pub input_file: InputFile,

    #[return_ref]
    pub errors: Vec<ParseError>,

    /// The items found in the file.
    #[return_ref]
    pub items: Vec<Item>,
}
