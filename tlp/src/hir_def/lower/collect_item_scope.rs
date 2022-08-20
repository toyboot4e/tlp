//! Lowers module item scope into [`Name`] → `Id<Loc<T>>` maps in [`ItemScope`]
//!
//! [`Name`]: crate::hir_def::ids::Name

use std::sync::Arc;

use la_arena::Arena;

use crate::hir_def::{
    db::{self, vfs::*},
    ids::{FileDataLoc, HirItemLoc},
    item_list::ItemScope,
    CrateData, FileData,
};

/// Collects tree of modules with `ItemScope`
pub fn lower_crate_data_query(db: &dyn db::Def, krate: VfsFileId) -> Arc<CrateData> {
    let mut modules = Arena::<FileData>::new();

    let root = {
        let idx = modules.alloc(FileData {
            file: krate.clone(),
            parent: None,
            children: Vec::new(),
            // use dummy item scope for now
            item_scope: Default::default(),
        });
        FileDataLoc { krate, idx }
    };

    // overwrite the dummy item scope
    modules[root.idx].item_scope = ModCollector {
        vfs_file_id: krate,
        root,
    }
    .module_item_scope(db);

    Arc::new(CrateData {
        root_file_idx: root,
        files: modules,
    })
}

struct ModCollector {
    vfs_file_id: VfsFileId,
    root: FileDataLoc,
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

            let id = db.intern_item_proc_loc(HirItemLoc {
                file: self.vfs_file_id,
                file_data: self.root,
                idx: id,
            });

            let _ix = scope.declare_proc(name, id);
        }

        Arc::new(scope)
    }
}
