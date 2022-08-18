//! Incremental computation powered by [`salsa`]

pub extern crate salsa;

pub mod ids;
pub mod vfs;

use std::sync::Arc;

use crate::{
    hir_def::{
        body::{Body, BodySourceMap, ItemSourceMap},
        expr, item, lower, scope, CrateData, ItemList,
    },
    syntax::ast::{self, ParseResult},
    utils::line_index::LineIndex,
};

use self::{ids::*, vfs::VfsFileId};

/// [`salsa`] database instance for the queries
#[salsa::database(SourceDB, ParseDB, InternDB, LowerModuleDB)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}

impl Upcast<dyn Intern> for DB {
    fn upcast(&self) -> &(dyn Intern + 'static) {
        &*self
    }
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

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

fn line_index(db: &dyn Source, file: VfsFileId) -> Arc<LineIndex> {
    let input = db.input(file);
    Arc::new(LineIndex::new(&input))
}

/// Parses source file into AST
#[salsa::query_group(ParseDB)]
pub trait Parse: Source {
    /// Parses the file into AST and returns AST and parse errors
    fn parse(&self, file: VfsFileId) -> Arc<ParseResult>;
}

fn parse(db: &dyn Parse, file: VfsFileId) -> Arc<ParseResult> {
    let src = db.input(file);
    let res = ast::parse(&src);
    Arc::new(res)
}

/// `hir_def` interner
#[salsa::query_group(InternDB)]
pub trait Intern: salsa::Database {
    // --------------------------------------------------------------------------------
    // AST locations
    // --------------------------------------------------------------------------------
    #[salsa::interned]
    fn intern_ast_block_loc(&self, loc: ids::AstLoc<ast::Block>) -> AstId<ast::Block>;

    // --------------------------------------------------------------------------------
    // Item locations
    // --------------------------------------------------------------------------------
    #[salsa::interned]
    fn intern_item_proc_loc(&self, proc: ItemLoc<item::DefProc>) -> ItemId<item::DefProc>;

    // --------------------------------------------------------------------------------
    // Path
    // --------------------------------------------------------------------------------
    // FIXME: use Arc-based interning
    #[salsa::interned]
    fn intern_path_data(&self, path: expr::PathData) -> Id<expr::PathData>;
}

/// Collecter of definitions of items
#[salsa::query_group(LowerModuleDB)]
pub trait Def: Parse + Intern + Upcast<dyn Intern> {
    // --------------------------------------------------------------------------------
    // File syntax
    // --------------------------------------------------------------------------------

    fn ast_id_map(&self, file_id: VfsFileId) -> Arc<ItemSourceMap>;

    #[salsa::invoke(lower::lower_crate_data_query)]
    fn crate_data(&self, krate: VfsFileId) -> Arc<CrateData>;

    #[salsa::invoke(lower::collect_file_item_list_query)]
    fn file_item_list(&self, file: VfsFileId) -> Arc<ItemList>;

    // --------------------------------------------------------------------------------
    // Body
    // --------------------------------------------------------------------------------

    #[salsa::invoke(lower::lower_proc_body_query)]
    fn proc_body(&self, proc_id: Id<ItemLoc<item::DefProc>>) -> Arc<Body>;

    #[salsa::invoke(lower::lower_proc_body_with_source_map_query)]
    fn proc_body_with_source_map(
        &self,
        proc_id: Id<ItemLoc<item::DefProc>>,
    ) -> (Arc<Body>, Arc<BodySourceMap>);

    #[salsa::invoke(scope::proc_expr_scope_query)]
    fn proc_expr_scope_map(&self, proc_id: Id<ItemLoc<item::DefProc>>) -> Arc<scope::ExprScopeMap>;

    // #[salsa::invoke(DefMap::block_def_map_query)]
    // fn block_item_list(&self, block: BlockId) -> Option<Arc<DefMap>>;
}

fn ast_id_map(db: &dyn Def, file_id: VfsFileId) -> Arc<ItemSourceMap> {
    let parse = db.parse(file_id);
    let map = ItemSourceMap::from_source(&parse.doc.syntax());

    Arc::new(map)
}
