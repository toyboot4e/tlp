/*!
`salsa` integration for incremental computation
*/

pub extern crate salsa;

pub mod ids;
pub mod input;

use std::sync::Arc;

use crate::{
    ir::{
        data::{def, DeclTree},
        db::input::FileId,
    },
    syntax::ast::{self, ParseResult},
    utils::line_index::LineIndex,
};

use self::ids::*;

/// `salsa` database for the [`queries`]
#[salsa::database(SourceDB, ParseDB, InternDB, LowerModuleDB)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}

/// Source data available to both compiler and IDE
///
/// NOTE: RA separates `SourceDatabase` and `FileLoader` from `SourceDatabaseExt` so that some
/// functionalities are not available in compiler-side code.
#[salsa::query_group(SourceDB)]
pub trait Source: salsa::Database {
    #[salsa::input]
    fn input(&self, file: FileId) -> Arc<String>;

    fn line_index(&self, file: FileId) -> Arc<LineIndex>;

    // #[salsa::input]
    // fn source_files(&self, krate: CrateLoc) -> ARc<Vec<Utf8PathBuf>>;
}

fn line_index(db: &dyn Source, file: FileId) -> Arc<LineIndex> {
    let input = db.input(file);
    Arc::new(LineIndex::new(&input))
}

/// Parses source file into AST
#[salsa::query_group(ParseDB)]
pub trait Parse: Source {
    /// Parses the file into AST and returns AST and parse errors
    fn parse(&self, file: FileId) -> Arc<ParseResult>;
}

fn parse(db: &dyn Parse, file: FileId) -> Arc<ParseResult> {
    let src = db.input(file);
    let res = ast::parse(&src);
    Arc::new(res)
}

/// Interner of definitions and locations
#[salsa::query_group(InternDB)]
pub trait Intern: salsa::Database {
    // Data → ID
    #[salsa::interned]
    fn intern_proc(&self, def: def::DefProc) -> ProcId;
    // location → IDs
}

/// Collecter of definitions of items
#[salsa::query_group(LowerModuleDB)]
pub trait Def: Parse + Intern {
    /// Creates an item tree for a file
    // TODO: Detect duplicate items?
    #[salsa::invoke(crate::ir::lower::item_tree_query)]
    fn item_tree(&self, file: FileId) -> Arc<DeclTree>;
    // fn proc_data(&self, id: FnId) -> Arc<FnDef>;
}

/// High-level inetrmediate representation
pub trait Hir: Def {
    // fn infer(&self, def: DefWithBodyId) -> Arc<InferenceResult>;
    // fn lower_struct(&self, def: Struct) -> Arc<LowerBatchResult>;
}
