//! Item scope

use rustc_hash::FxHashMap;

use base::jar::Word;

use crate::ir::item;

/// Items visible in a scope (declarations and imports)
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ItemScope {
    // Declarations
    procs: FxHashMap<Word, item::Proc>,
}

impl ItemScope {
    pub(crate) fn declare_proc(&mut self, name: Word, proc: item::Proc) {
        // TOOD: consider upcasting or not
        // let id = DefId { loc_id: proc };
        // self.procs.insert(name, AnyDefId::from(id));
        self.procs.insert(name, proc);
    }

    pub fn lookup_proc(&self, name: &Word) -> Option<item::Proc> {
        self.procs.get(name).cloned()
    }
}
