//! Lowers module hierarchy

use std::sync::Arc;

use la_arena::Arena;

use crate::hir_def::{
    db::{
        self,
        ids::{Loc, TreeId},
        vfs::*,
    },
    decl::Visibility,
    CrateDefMap, ItemScope, ModuleData,
};

/// Collects tree of modules with `ItemScope`
pub fn def_map_query(db: &dyn db::Def, krate: FileId) -> Arc<CrateDefMap> {
    let mut modules = Arena::<ModuleData>::new();

    let root_item_tree = db.file_item_tree(krate.clone());
    let tree_id = TreeId::new(krate);
    let root_scope = ModCollector { tree_id }.module_item_scope(db);

    let root = modules.alloc(ModuleData {
        file: krate.clone(),
        vis: Visibility::Public,
        parent: None,
        children: Vec::new(),
        scope: root_scope,
    });

    Arc::new(CrateDefMap { root, modules })
}

struct ModCollector {
    tree_id: TreeId,
}

impl ModCollector {
    /// Visits module items and makes up the scope
    fn module_item_scope(&self, db: &dyn db::Def) -> Arc<ItemScope> {
        let item_tree = self.tree_id.item_tree(db);
        let mut scope = ItemScope::default();

        for (id, proc) in item_tree.procs().iter() {
            let name = match &proc.name {
                Some(name) => name.clone(),
                None => continue,
            };

            let id = db.intern_proc(Loc {
                tree: self.tree_id,
                item: id,
            });

            let ix = scope.declare_proc(name, id);
        }

        Arc::new(scope)
    }
}
