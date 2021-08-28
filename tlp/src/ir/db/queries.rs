/*!
`salsa` query groups and implementations

Most data types in this module are automatically generated by `salsa` macros.

# Main query types

* [`Source`]: FileId → String
* [`Parse`]: String → AST
* [`Intern`]: Path → ID
* [`Def`]: AST → DefMap
*/

use std::sync::Arc;

use camino::Utf8Path;

use crate::{
    ir::{
        data::{def, loc::*},
        db::ids::*,
        lower::LowerError,
        tree::{CrateTree, ModuleTree},
    },
    syntax::ast::{self, ParseResult},
    utils::line_index::LineIndex,
};

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

#[salsa::query_group(ParseDB)]
pub trait Parse: Source {
    /// The parsed form of the request.
    fn parse(&self, file: FileId) -> Arc<ParseResult>;
}

fn parse(db: &dyn Parse, file: FileId) -> Arc<ParseResult> {
    let src = db.input(file);
    let res = ast::parse(&src);
    Arc::new(res)
}

/// Interner of locations
#[salsa::query_group(InternDB)]
pub trait Intern: salsa::Database {
    #[salsa::interned]
    fn intern_access(&self, access: AbsAccess) -> AccessId;
    #[salsa::interned]
    fn intern_proc(&self, def: def::DefProc) -> ProcId;
    #[salsa::interned]
    fn intern_module_loc(&self, module: ModuleLoc) -> ModuleId;
    #[salsa::interned]
    fn intern_crate_loc(&self, krate: CrateLoc) -> CrateId;
}

/// Collecter of definitions of items
#[salsa::query_group(LowerModuleDB)]
pub trait Def: Parse + Intern {
    /// Collects items in a module into a tree
    fn item_tree(&self, module: ModuleId) -> Arc<ModuleLower>;
    // fn proc_data(&self, id: FnId) -> Arc<FnDef>;
}

fn item_tree(db: &dyn Def, module: ModuleId) -> Arc<ModuleLower> {
    let module_loc = db.lookup_intern_module_loc(module);

    let file = module_loc.to_file().unwrap();
    let ast = db.parse(file).doc.clone();

    let krate = module_loc.krate().clone();
    let tree = ModuleTree::crate_root(krate, module);

    let mut lower = ModuleLower::new(tree);
    lower.lower(db, &module_loc.access().to_path_buf(), ast);
    Arc::new(lower)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleLower {
    pub errs: Vec<LowerError>,
    pub tree: ModuleTree,
}

impl ModuleLower {
    fn new(tree: ModuleTree) -> Self {
        Self {
            errs: Vec::new(),
            tree,
        }
    }

    fn lower(&mut self, db: &dyn Def, module_path: &Utf8Path, ast: ast::data::Document) {
        self.collect_procs(db, module_path, ast.item_nodes());
    }

    fn collect_procs(
        &mut self,
        db: &dyn Def,
        module_path: &Utf8Path,
        forms: impl Iterator<Item = ast::data::Form>,
    ) {
        for form in forms {
            if let Some(ast) = form.as_proc() {
                // TODO: validate name?
                let tk = ast.name_tk();
                let name = tk.text();
                let access_data = AbsAccess::new(module_path.join(name));
                let access = db.intern_access(access_data);

                if let Some(def_proc) = self.lower_proc(db, access, ast) {
                    self.tree.procs.push(def_proc);
                }
            }
        }
    }

    fn lower_proc(
        &mut self,
        db: &dyn Def,
        access: AccessId,
        ast: ast::data::DefProc,
    ) -> Option<def::DefProc> {
        if self.tree.procs.iter().any(|p| p.access == access) {
            self.errs.push(LowerError::DupProc {
                access: db.lookup_intern_access(access),
                s: ast.name_tk().text().to_string(),
            });
            return None;
        }

        let proc = def::DefProc::new(ast, access);
        Some(proc)
    }
}

#[salsa::query_group(LowerCrateDB)]
pub trait LowerCrate: Def {
    fn crate_tree(&self, krate: CrateLoc) -> CrateTree;
}

fn crate_tree(db: &dyn LowerCrate, krate: CrateLoc) -> CrateTree {
    todo!()
}
