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

/// Tree item location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc<T> {
    /// → FileItemList
    pub file: VfsFileId,
    /// → T
    pub idx: Idx<T>,
}

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

/// Definition ID
///
/// It's not a location unlike RA.
#[derive(Derivative)]
#[derivative(Debug, Hash, PartialEq, Eq)]
pub struct DefId<T> {
    pub loc_id: Id<Loc<T>>,
    // TODO: add ModuleId
    // mod_: ModuleId,
}

impl<T> Clone for DefId<T> {
    fn clone(&self) -> Self {
        Self {
            loc_id: self.loc_id,
        }
    }
}

impl<T> Copy for DefId<T> {}

#[derive(Derivative, Copy)]
#[derivative(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnyDefId {
    Proc(DefId<item::DefProc>),
}

macro_rules! impl_from {
    ($Ty:ty, $Var:ident) => {
        impl From<$Ty> for AnyDefId {
            fn from(x: $Ty) -> Self {
                Self::$Var(x)
            }
        }
    };
}

impl_from!(DefId<item::DefProc>, Proc);

impl Id<Loc<item::DefProc>> {
    // NOTE: Use `Def` database, not `Intern` database as parameter. This is because Rust doesn't
    // have upcasting coercion (yet).
    pub fn lookup(&self, db: &dyn db::Def) -> Loc<item::DefProc> {
        db.lookup_intern_proc_loc(*self)
    }
}

// mistake: data → ID
//     ProcId decl::DefProc, proc "Newtype of interned ID for procedure",
