use std::sync::Arc;

use la_arena::{Arena, Idx};

use crate::{
    ir::{
        data::{
            body::*,
            decl::{self, ItemTree},
            def,
            res::{CrateDefMap, ItemScope, ModuleData},
        },
        db::{
            self,
            ids::{Id, Loc},
            vfs::*,
        },
    },
    syntax::ast::data as ast,
};

use super::data::decl::Visibility;

/// Collets syntax-reflected form of declarations and imports
pub(crate) fn item_tree_query(db: &dyn db::Def, file: FileId) -> Arc<ItemTree> {
    let ast = db.parse(file.clone()).doc.clone();
    Arc::new(ItemTreeCollect::run(file, ast))
}

struct ItemTreeCollect {
    tree: ItemTree,
}

impl ItemTreeCollect {
    fn run(file: FileId, ast: ast::Document) -> ItemTree {
        let mut me = Self {
            tree: ItemTree::new(file),
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

/// Collects tree of modules with `ItemScope`
pub(crate) fn def_map_query(db: &dyn db::Def, krate: FileId) -> Arc<CrateDefMap> {
    let mut modules = Arena::<ModuleData>::new();

    let root_item_tree = db.file_item_tree(krate.clone());
    let root_scope = self::module_item_scope(db, &root_item_tree);

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
fn module_item_scope(db: &dyn db::Def, item_tree: &ItemTree) -> Arc<ItemScope> {
    let mut scope = ItemScope::default();

    for (ix, proc) in item_tree.procs().iter() {
        let name = proc.name.clone();
        scope.declare_proc(name, ix);
    }

    Arc::new(scope)
}

pub(crate) fn proc_data_query(
    db: &dyn db::Def,
    proc: Id<Loc<def::ProcData>>,
) -> Arc<def::ProcData> {
    todo!()
}

pub(crate) fn lower_proc_body(db: &dyn db::Def, proc_id: Id<Loc<decl::DefProc>>) -> Arc<Body> {
    // collect parameters as pattern IDs

    // body = block expr
    let body = {
        let mut lower = LowerExpr {
            db,
            body: Body::default(),
        };

        let proc_loc = db.lookup_intern_proc(proc_id);
        let tree = proc_loc.tree.item_tree(db);
        let proc = &tree[proc_loc.item];
        lower.lower_proc(proc.ast.clone());

        lower.body
    };

    Arc::new(body)
}

/// Proc AST → Proc HIR
struct LowerExpr<'a> {
    db: &'a dyn db::Def,
    body: Body,
}

impl<'a> LowerExpr<'a> {
    pub fn lower_proc(&mut self, proc: ast::DefProc) {
        self.lower_proc_params(proc.clone());
        self.lower_proc_body(proc.clone());
    }

    fn lower_proc_params(&mut self, proc: ast::DefProc) {
        if proc.params().is_some() {
            // lower self parameter
            // lower other parameters
        }
    }

    fn lower_proc_body(&mut self, proc: ast::DefProc) {
        // TODO: Consider block modifier (e.g. coroutines)

        for form in proc.body_forms() {
            //
        }
    }
}
