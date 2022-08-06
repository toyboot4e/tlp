//! Lowers module items

use std::sync::Arc;

use crate::{
    hir_def::{
        db::{self, vfs::*},
        decl::{self, ItemTree},
        def,
        def::*,
        item::expr::Expr,
        CrateDefMap, ItemScope, ModuleData,
    },
    syntax::ast,
};

/// Collects declarations and imports; they're interned, but as-is
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
            match form.kind() {
                ast::FormKind::DefProc(ast_proc) => {
                    let hir_proc = self.lower_proc(ast_proc);
                    self.tree.procs.alloc(hir_proc);
                    continue;
                }
                _ => {}
            }
        }
    }

    fn lower_proc(&mut self, ast: ast::DefProc) -> decl::DefProc {
        decl::DefProc::from_ast(ast)
    }
}
