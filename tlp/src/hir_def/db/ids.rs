/*!
IDs of interned data types

# Data flows

ID ← Loc ← Data:
* Interning: Loc → ID

ID → Loc → Data:
* lookup: ID → Loc
* container: Loc → Container → Data
*/

use std::marker::PhantomData;

use derivative::Derivative;
use la_arena::Idx;

use crate::hir_def::{
    db::{self, vfs::VfsFileId},
    item,
};

/// HIR item location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ItemLoc<T> {
    /// Index to (TODO: what?)
    pub file: VfsFileId,
    /// Index to [`ItemList`](crate::hir_def::scope::ItemList)
    pub idx: Idx<T>,
}

// /// AST syntax location
// #[derive(Debug, Hash, PartialEq, Eq, Clone)]
// pub struct AstLoc<T> {
//     pub file: VfsFileId,
//     /// FIXME: Index to AstIdMap
//     pub idx: Idx<T>,
//     // /// The containing module.
//     // module: ModuleId,
// }

/// Interned ID to a location
#[derive(Derivative)]
#[derivative(Debug, Hash, PartialEq, Eq)]
pub struct Id<T> {
    raw: salsa::InternId,
    _ty: PhantomData<T>,
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw,
            _ty: PhantomData,
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> salsa::InternKey for Id<T> {
    fn from_intern_id(id: salsa::InternId) -> Self {
        Self {
            raw: id,
            _ty: PhantomData,
        }
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.raw
    }
}

impl Id<ItemLoc<item::DefProc>> {
    // NOTE: Use `Def` database, not `Intern` database as parameter. This is because Rust doesn't
    // have upcasting coercion (yet).
    pub fn lookup(&self, db: &dyn db::Def) -> ItemLoc<item::DefProc> {
        db.lookup_intern_proc_loc(*self)
    }
}

// mistake: data → ID
//     ProcId decl::DefProc, proc "Newtype of interned ID for procedure",
