/*!
Lowers AST into item tree
*/

use std::sync::Arc;

use crate::{
    ir::{
        data::{def, DeclTree},
        db::{self, input::*},
    },
    syntax::ast::data as ast,
};

/// Creates an [`ItemTree`] for given [`FileId`]
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

    fn lower_proc(&mut self, ast: ast::DefProc) -> def::DefProc {
        def::DefProc::from_ast(ast)
    }
}
