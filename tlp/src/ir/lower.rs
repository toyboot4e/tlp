/*!
Lowers AST into item tree
*/

use std::sync::Arc;

use crate::{
    ir::{
        data::{def, loc::*},
        db,
        tree::ItemTree,
    },
    syntax::ast::data as ast,
};

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

        me.collect_procs(ast.item_nodes());

        me.tree
    }

    fn collect_procs(&mut self, forms: impl Iterator<Item = ast::Form>) {
        for form in forms {
            if let Some(ast) = form.as_proc() {
                if let Some(def_proc) = self.lower_proc(ast) {
                    self.tree.procs.push(def_proc);
                }
            }
        }
    }

    fn lower_proc(&mut self, ast: ast::DefProc) -> Option<def::DefProc> {
        let proc = def::DefProc::new(ast);
        Some(proc)
    }
}
