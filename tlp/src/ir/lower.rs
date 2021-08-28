/*!
Lowers AST into an item tree

# The lowering pass

Item definitions are collected into `DefMap`?

* The output of the pass is the input to the name resolution pass
* Locations are interned with salsa database (for smaller use of memory)
*/

use std::sync::Arc;

use crate::{
    ir::{
        data::{def, loc::*},
        db::queries,
        tree::ItemTree,
    },
    syntax::ast::data as ast,
};

pub(crate) fn item_tree_query(db: &dyn queries::Def, file: FileId) -> Arc<ItemTree> {
    let ast = db.parse(file).doc.clone();
    Arc::new(ItemTreeCollect::run(ast))
}

struct ItemTreeCollect {
    tree: ItemTree,
}

impl ItemTreeCollect {
    fn run(ast: ast::Document) -> ItemTree {
        let mut me = Self {
            tree: ItemTree::new(),
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
