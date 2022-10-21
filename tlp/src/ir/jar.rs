//! Jars

use base::jar::InputFile;

use crate::{
    ir::{item::Item, IrJar},
    syntax::cst::{lex::LexError, ParseError},
};

/// File parsed into items
#[salsa::tracked(jar = IrJar)]
pub struct ParsedFile {
    #[id]
    pub input_file: InputFile,

    /// If non-empty, the parse stopped on lexical errors
    #[return_ref]
    pub lex_errors: Vec<LexError>,

    #[return_ref]
    pub errors: Vec<ParseError>,

    /// The items found in the file.
    #[return_ref]
    pub items: Vec<Item>,
}
