//! Lowers module items into `Arena` s in `ItemList`

use std::sync::Arc;

use crate::{
    hir_def::{
        body::AstIdMap,
        db::{self, vfs::*},
        item, ItemList,
    },
    syntax::ast,
};

/// Collects declarations and imports; they're interned, but as-is
pub fn collect_file_item_list_query(db: &dyn db::Def, file: VfsFileId) -> Arc<ItemList> {
    let ast = db.parse(file.clone()).doc.clone();
    let ast_id_map = db.ast_id_map(file);
    Arc::new(ItemListCollect::run(file, ast, ast_id_map))
}

struct ItemListCollect {
    tree: ItemList,
    ast_id_map: Arc<AstIdMap>,
}

impl ItemListCollect {
    fn run(file: VfsFileId, ast: ast::Document, ast_id_map: Arc<AstIdMap>) -> ItemList {
        let mut me = Self {
            tree: ItemList::new(file),
            ast_id_map,
        };

        me.collect(ast.item_nodes());

        me.tree
    }

    fn collect(&mut self, items: impl Iterator<Item = ast::Item>) {
        for item in items {
            match item {
                ast::Item::DefProc(ast_proc) => {
                    let hir_proc = self.lower_proc(ast_proc);
                    self.tree.procs.alloc(hir_proc);
                    continue;
                }
            }
        }
    }

    fn lower_proc(&mut self, ast: ast::DefProc) -> item::DefProc {
        let ast_id = self.ast_id_map.ast_to_idx(&ast);
        item::DefProc::from_ast(ast, ast_id)
    }
}
