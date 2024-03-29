//! toylisp
//!
//! The implementation is heavily based on [`salsa`] (`salsa-2022`). Without knowing how to use it,
//! the documentation and the source code would not make much sense :)

pub extern crate base;

pub mod compile;
pub mod ir;
pub mod syntax;
pub mod util;
pub mod vm;

use base::jar::*;

#[salsa::db(base::BaseJar, ir::IrJar)]
#[derive(Default)]
pub struct Db {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Db {}

impl Db {
    pub fn new_input_file(&mut self, name: impl ToString, source_text: String) -> InputFile {
        let name = Word::intern(self, name);
        InputFile::new(self, name, source_text)
    }

    // pub fn diagnostics(&self, input_file: InputFile) -> Vec<Diagnostic> {}

    // pub fn main_function(&self, input_file: InputFile) -> Option<Bir> { }

    // /// Parses `input_file` and returns a list of the items within.
    // pub fn debug_syntax_tree(&self, item: Item) -> Option<impl std::fmt::Debug + '_> {
    //     Some(item.syntax_tree(self)?.into_debug(self))
    // }
}
