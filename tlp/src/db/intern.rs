/*!
IDs of interned data types
*/

use crate::ir::lower::{def::DefProc, loc::*};

/// Interner of locations
#[salsa::query_group(InternDB)]
pub trait Intern: salsa::Database {
    #[salsa::interned]
    fn intern_access(&self, access: AbsAccess) -> Access;
    #[salsa::interned]
    fn intern_proc(&self, def: DefProc) -> Proc;
    #[salsa::interned]
    fn intern_module_token(&self, module: ModuleLoc) -> Module;
    #[salsa::interned]
    fn intern_crate_token(&self, krate: CrateLoc) -> Crate;
}

macro_rules! keys {
    ($($id:ident $data:ident $intern:ident $doc:expr,)*) => {
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
                pub fn lookup(&self, db: &dyn Intern) -> $data {
                    paste::paste! {
                        db. [<lookup_intern_ $intern>] (*self)
                    }
                }
            }
        )*
    };
}

keys! {
    Proc DefProc proc "Typed interned ID of procedure",
    Access AbsAccess access "Typed interned ID of path",
    Module ModuleLoc module_token "Typed interned ID of module token",
    Crate CrateLoc crate_token "Typed interned ID of crate token",
}
