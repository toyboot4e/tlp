//! Lowers module hierarchy

use std::sync::Arc;

use la_arena::Arena;

use crate::hir_def::{
    db::{self, ids::ItemLoc, vfs::*},
    CrateData, FileData, FileDataId, ItemScope,
};

/// Collects tree of modules with `ItemScope`
pub fn crate_data_query(db: &dyn db::Def, krate: VfsFileId) -> Arc<CrateData> {
    let mut modules = Arena::<FileData>::new();

    let root_scope = ModCollector { vfs_file_id: krate }.module_item_scope(db);

    let root = FileDataId {
        idx: modules.alloc(FileData {
            file: krate.clone(),
            parent: None,
            children: Vec::new(),
            item_scope: root_scope,
        }),
    };

    Arc::new(CrateData {
        root,
        files: modules,
    })
}

struct ModCollector {
    vfs_file_id: VfsFileId,
}

impl ModCollector {
    /// Visits module items and makes up the scope
    fn module_item_scope(&self, db: &dyn db::Def) -> Arc<ItemScope> {
        let item_tree = db.file_item_list(self.vfs_file_id);
        let mut scope = ItemScope::default();

        for (id, proc) in item_tree.procs().iter() {
            let name = match &proc.name {
                Some(name) => name.clone(),
                None => continue,
            };

            let id = db.intern_proc_loc(ItemLoc {
                file: self.vfs_file_id,
                idx: id,
            });

            let _ix = scope.declare_proc(name, id);
        }

        Arc::new(scope)
    }
}
