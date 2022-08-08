//! Lowers module items

use std::sync::Arc;

use crate::{
    hir_def::{
        db::{self, vfs::*},
        item, ItemList,
    },
    syntax::ast,
};

/// Collects declarations and imports; they're interned, but as-is
pub fn file_item_list_query(db: &dyn db::Def, file: VfsFileId) -> Arc<ItemList> {
    let ast = db.parse(file.clone()).doc.clone();
    Arc::new(ItemTreeCollect::run(file, ast))
}

struct ItemTreeCollect {
    tree: ItemList,
}

impl ItemTreeCollect {
    fn run(file: VfsFileId, ast: ast::Document) -> ItemList {
        let mut me = Self {
            tree: ItemList::new(file),
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

    fn lower_proc(&mut self, ast: ast::DefProc) -> item::DefProc {
        item::DefProc::from_ast(ast)
    }
}
