use std::sync::Arc;

use crate::{
    ir::{
        data::{
            body::*,
            decl::{self, DeclTree},
            res::CrateDefMap,
        },
        db::{self, ids::*, input::*},
    },
    syntax::ast::data as ast,
};

/// Collects item declarations and makes up a tree
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
                self.tree.procs.push(def_proc);
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

pub(crate) fn def_map_query(db: &dyn db::Def, tree: Arc<DeclTree>) -> Arc<CrateDefMap> {
    // name-resolution loop to the fixed point
    // loop {
    //     // resolve imports and macros
    // }

    todo!()
}

fn collect_defs(db: &dyn db::Def, map: &mut CrateDefMap) {
    todo!()
}

pub(crate) fn lower_proc_body(db: &dyn db::Def, proc: ProcId) -> Arc<Body> {
    todo!()
}
