//! Incremental computation powered by [`salsa`]

pub extern crate salsa;

pub mod ids;
pub mod vfs;

use std::sync::Arc;

use crate::{
    hir_def::{
        decl::{self, ItemTree},
        def::{self, Body},
        lower,
        res::CrateDefMap,
    },
    syntax::ast::{self, ParseResult},
    utils::line_index::LineIndex,
};

use self::{
    ids::{DefId, Id, Loc},
    vfs::FileId,
};

/// [`salsa`] database for the `queries`
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

/// Parses source file into AST
#[salsa::query_group(ParseDB)]
pub trait Parse: Source {
    /// Parses the file into AST and returns AST and parse errors
    fn parse(&self, file: FileId) -> Arc<ParseResult>;
}

/// Interner of locations (`Loc<T>` → `Id<Loc<T>>` and vice versa)
#[salsa::query_group(InternDB)]
pub trait Intern: salsa::Database {
    #[salsa::interned]
    fn intern_proc(&self, proc: Loc<decl::DefProc>) -> Id<Loc<decl::DefProc>>;
}

/// Collecter of definitions of items
#[salsa::query_group(LowerModuleDB)]
pub trait Def: Parse + Intern {
    /// Collects declarations in a module. This contains unresolved imports
    #[salsa::invoke(crate::hir_def::lower::item_tree_query)]
    fn file_item_tree(&self, file: FileId) -> Arc<ItemTree>;

    // TODO: duplicate item diagnostics

    /// Collects module items and makes up a tree. All imports in the underlying `ItemTree` are
    /// resolved in `ItemScope`.
    #[salsa::invoke(crate::hir_def::lower::def_map_query)]
    fn crate_def_map(&self, krate: FileId) -> Arc<CrateDefMap>;

    // #[salsa::invoke(DefMap::block_def_map_query)]
    // fn block_def_map(&self, block: BlockId) -> Option<Arc<DefMap>>;

    #[salsa::invoke(lower::proc_data_query)]
    fn proc_data(&self, proc_id: Id<Loc<decl::DefProc>>) -> Arc<def::ProcData>;

    #[salsa::invoke(lower::proc_body_query)]
    fn proc_body(&self, proc_id: Id<Loc<decl::DefProc>>) -> Arc<Body>;
}

fn line_index(db: &dyn Source, file: FileId) -> Arc<LineIndex> {
    let input = db.input(file);
    Arc::new(LineIndex::new(&input))
}

fn parse(db: &dyn Parse, file: FileId) -> Arc<ParseResult> {
    let src = db.input(file);
    let res = ast::parse(&src);
    Arc::new(res)
}
