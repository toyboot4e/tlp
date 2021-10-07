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

/// Identifier of `ItemTree`
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TreeId {
    file: FileId,
}

impl TreeId {
    pub fn item_tree(&self, db: &dyn db::Def) -> Arc<decl::ItemTree> {
        db.file_item_tree(self.file)
    }
}

/// Identifier of `ItemTree` item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc<T> {
    pub tree: TreeId,
    pub item: Idx<T>,
}

/// Interned data ID
#[derive(Derivative)]
#[derivative(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Id<T> {
    raw: salsa::InternId,
    _ty: PhantomData<T>,
}

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

impl Id<Loc<decl::DefProc>> {
    // NOTE: Use `Def` database, not `Intern` database as parameter. This is because Rust doesn't
    // have upcasting coercion (yet).
    pub fn lookup(&self, db: &dyn db::Def) -> Loc<decl::DefProc> {
        db.lookup_intern_proc(*self)
    }
}

// loc → ID
// new_ids! {
//     Path
// }

// mistake: data → ID
//     ProcId decl::DefProc, proc "Newtype of interned ID for procedure",
