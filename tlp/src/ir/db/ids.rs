/*!
IDs of interned data types

# Data flows

ID ← Loc ← Data:
* Interning: Loc → ID

ID → Loc → Data:
* lookup: ID → Loc
* container: Loc → Container → Data
*/

use std::{marker::PhantomData, sync::Arc};

use derivative::Derivative;
use la_arena::Idx;

use crate::ir::{
    data::decl,
    db::{self, vfs::FileId, Intern},
};

macro_rules! new_ids {
    ($($id:ident $decl:path, $intern:ident $doc:expr,)*) => {
        $(
            #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
            #[doc = $doc]
            pub struct $id {
                raw: salsa::InternId,
            }

            impl salsa::InternKey for $id {
                fn from_intern_id(id: salsa::InternId) -> Self {
                    Self { raw: id }
                }

                fn as_intern_id(&self) -> salsa::InternId {
                    self.raw
                }
            }

            impl $id {
                /// Calls corresponding lookup function of DB
                pub fn lookup(&self, db: &dyn Intern) -> $decl {
                    paste::paste! {
                        db. [<lookup_intern_ $intern>] (*self)
                    }
                }
            }
        )*
    };
}

// // loc → ID
// new_ids! {
//     Path
// }

/// Identifier of an `ItemTree`
#[derive(Copy, Debug, PartialEq, Eq, Clone, Hash)]
pub struct TreeId {
    file: FileId,
}

impl TreeId {
    pub fn new(file: FileId) -> Self {
        Self { file }
    }

    pub fn item_tree(&self, db: &dyn db::Def) -> Arc<decl::ItemTree> {
        db.file_item_tree(self.file)
    }
}

/// Tree item location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc<T> {
    /// ID of the tree
    pub tree: TreeId,
    /// ID of the item in the tree
    pub item: Idx<T>,
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
    Proc(DefId<decl::DefProc>),
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

impl_from!(DefId<decl::DefProc>, Proc);

impl Id<Loc<decl::DefProc>> {
    // NOTE: Use `Def` database, not `Intern` database as parameter. This is because Rust doesn't
    // have upcasting coercion (yet).
    pub fn lookup(&self, db: &dyn db::Def) -> Loc<decl::DefProc> {
        db.lookup_intern_proc(*self)
    }
}

// mistake: data → ID
//     ProcId decl::DefProc, proc "Newtype of interned ID for procedure",
