//! Incremental computation powered by [`salsa`]

pub extern crate salsa;

pub mod ids;
pub mod vfs;

use std::sync::Arc;

use crate::{
    hir_def::{body::Body, item, lower, scope, CrateData, ItemList},
    syntax::ast::{self, ParseResult},
    utils::line_index::LineIndex,
};

use self::{
    ids::{Id, ItemLoc},
    vfs::VfsFileId,
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
    fn input(&self, file: VfsFileId) -> Arc<String>;

    fn line_index(&self, file: VfsFileId) -> Arc<LineIndex>;

    // #[salsa::input]
    // fn source_files(&self, krate: CrateLoc) -> ARc<Vec<Utf8PathBuf>>;
}

/// Parses source file into AST
#[salsa::query_group(ParseDB)]
pub trait Parse: Source {
    /// Parses the file into AST and returns AST and parse errors
    fn parse(&self, file: VfsFileId) -> Arc<ParseResult>;
}

/// Interner of locations (`Loc<T>` → `Id<Loc<T>>` and vice versa)
#[salsa::query_group(InternDB)]
pub trait Intern: salsa::Database {
    #[salsa::interned]
    fn intern_proc_loc(&self, proc: ItemLoc<item::DefProc>) -> Id<ItemLoc<item::DefProc>>;
    // #[salsa::interned]
    // fn intern_block_loc(&self, proc: AstLoc<ast::Block>) -> Id<AstLoc<item::DefProc>>;
}

// pub trait Ast: Parse + Intern { }

/// Collecter of definitions of items
#[salsa::query_group(LowerModuleDB)]
pub trait Def: Parse + Intern {
    // --------------------------------------------------------------------------------
    // File syntax
    // --------------------------------------------------------------------------------

    #[salsa::invoke(lower::crate_data_query)]
    fn crate_data(&self, krate: VfsFileId) -> Arc<CrateData>;

    #[salsa::invoke(lower::file_item_list_query)]
    fn file_item_list(&self, file: VfsFileId) -> Arc<ItemList>;

    // --------------------------------------------------------------------------------
    // Body
    // --------------------------------------------------------------------------------

    #[salsa::invoke(lower::proc_body_query)]
    fn proc_body(&self, proc_id: Id<ItemLoc<item::DefProc>>) -> Arc<Body>;

    #[salsa::invoke(scope::proc_expr_scope_query)]
    fn proc_expr_scopes(&self, proc_id: Id<ItemLoc<item::DefProc>>) -> Arc<scope::ExprScopeMap>;

    // #[salsa::invoke(DefMap::block_def_map_query)]
    // fn block_item_list(&self, block: BlockId) -> Option<Arc<DefMap>>;
}

fn line_index(db: &dyn Source, file: VfsFileId) -> Arc<LineIndex> {
    let input = db.input(file);
    Arc::new(LineIndex::new(&input))
}

fn parse(db: &dyn Parse, file: VfsFileId) -> Arc<ParseResult> {
    let src = db.input(file);
    let res = ast::parse(&src);
    Arc::new(res)
}
