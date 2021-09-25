use std::sync::Arc;

use la_arena::{Arena, Idx};

use crate::{
    ir::{
        data::{
            body::*,
            decl::{self, DeclTree},
            res::{CrateDefMap, ItemScope, ModuleData},
        },
        db::{self, input::*},
    },
    syntax::ast::data as ast,
};

use super::data::decl::Visibility;

/// Queries a syntax-reflected form of declarations and imports
pub(crate) fn item_tree_query(db: &dyn db::Def, file: FileId) -> Arc<DeclTree> {
    let ast = db.parse(file.clone()).doc.clone();
    Arc::new(ItemTreeCollect::run(file, ast))
}

struct ItemTreeCollect {
    tree: DeclTree,
}

impl ItemTreeCollect {
    fn run(file: FileId, ast: ast::Document) -> DeclTree {
        let mut me = Self {
            tree: DeclTree::new(file),
        };

        me.collect(ast.item_nodes());

        me.tree
    }

    fn collect(&mut self, forms: impl Iterator<Item = ast::Form>) {
        for form in forms {
            if let Some(ast) = form.as_proc() {
                let def_proc = self.lower_proc(ast);
                self.tree.procs.alloc(def_proc);
                continue;
            }

            // if let Some(ast) = form.as_data() {
            // }
        }
    }

    fn lower_proc(&mut self, ast: ast::DefProc) -> decl::DefProc {
        decl::DefProc::from_ast(ast)
    }
}

/// Queries top-level name resolutions (name-declaration map and item scopes)
pub(crate) fn def_map_query(db: &dyn db::Def, krate: FileId) -> Arc<CrateDefMap> {
    let mut modules = Arena::<ModuleData>::new();

    let root_decls = db.decl_tree(krate.clone());
    let root_scope = self::module_scope(db, &root_decls);

    let root = modules.alloc(ModuleData {
        file: krate.clone(),
        vis: Visibility::Public,
        parent: None,
        children: Vec::new(),
        scope: root_scope,
    });

    Arc::new(CrateDefMap { root, modules })
}

/// Visits module items and makes up the scope
fn module_scope(db: &dyn db::Def, decls: &DeclTree) -> Arc<ItemScope> {
    let mut scope = ItemScope::default();

    for (ix, proc) in decls.procs().iter() {
        let name = proc.name.clone();
        scope.declare_proc(name, ix);
    }

    Arc::new(scope)
}

pub(crate) fn lower_proc_body(db: &dyn db::Def, proc: Idx<decl::DefProc>) -> Arc<Body> {
    // collect parameters as pattern IDs

    // body = block expr

    todo!()
}

struct LowerExpr<'a> {
    db: &'a dyn db::Def,
}

impl<'a> LowerExpr<'a> {
    //
}
