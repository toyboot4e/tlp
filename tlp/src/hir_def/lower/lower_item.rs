//! Lowers module items into `Arena` s in `ItemList`

use std::sync::Arc;

use crate::{
    hir_def::{
        body::ItemSourceMap,
        db::{self, vfs::*},
        item_list::{item, ItemList},
    },
    syntax::ast,
};

/// Collects declarations and imports; they're interned, but as-is
pub fn collect_file_item_list_query(db: &dyn db::Def, file: VfsFileId) -> Arc<ItemList> {
    let ast = db.parse(file.clone()).doc.clone();
    let item_source_map = db.item_source_map(file);
    Arc::new(ItemListCollect::run(file, ast, item_source_map))
}

struct ItemListCollect {
    tree: ItemList,
    item_source_map: Arc<ItemSourceMap>,
}

impl ItemListCollect {
    fn run(file: VfsFileId, ast: ast::Document, ast_id_map: Arc<ItemSourceMap>) -> ItemList {
        let mut me = Self {
            tree: ItemList::new(file),
            item_source_map: ast_id_map,
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
        let ast_id = self.item_source_map.ptr_to_idx(&ast);
        item::DefProc::from_ast(ast, ast_id)
    }
}
