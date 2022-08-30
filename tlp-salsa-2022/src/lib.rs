//! toylisp

pub extern crate base;

pub mod ir;
pub mod syntax;

use base::{jar::*, span::*};

use crate::ir::{InputFileExt, item::Item};

#[salsa::db(base::BaseJar, ir::IrJar)]
#[derive(Default)]
pub struct Db {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Db {}

// TODO: Send
// impl salsa::ParallelDatabase for Db {
//     fn snapshot(&self) -> salsa::Snapshot<Self> {
//         salsa::Snapshot::new(Db {
//             storage: self.storage.snapshot(),
//         })
//     }
// }

impl Db {
    pub fn new_input_file(&mut self, name: impl ToString, source_text: String) -> InputFile {
        let name = Word::intern(self, name);
        InputFile::new(self, name, source_text)
    }

    // pub fn diagnostics(&self, input_file: InputFile) -> Vec<Diagnostic> {}

    // pub fn main_function(&self, input_file: InputFile) -> Option<Bir> { }

    /// Parses `input_file` and returns a list of the items within.
    pub fn items(&self, input_file: InputFile) -> &[Item] {
        input_file.items(self)
    }

    // /// Parses `input_file` and returns a list of the items within.
    // pub fn debug_syntax_tree(&self, item: Item) -> Option<impl std::fmt::Debug + '_> {
    //     Some(item.syntax_tree(self)?.into_debug(self))
    // }

    /// Converts a given offset in a given file into line/column information.
    pub fn line_column(&self, input_file: InputFile, offset: Offset) -> LineColumn {
        base::ln::line_column(self, input_file, offset)
    }

    /// Converts a `FileSpan` into its constituent parts.
    pub fn line_columns(&self, span: FileSpan) -> (InputFile, LineColumn, LineColumn) {
        let start = self.line_column(span.input_file, span.start);
        let end = self.line_column(span.input_file, span.end);
        (span.input_file, start, end)
    }
}
