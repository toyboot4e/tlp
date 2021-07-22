/*!
IDs of interned data types
*/

use crate::{
    db::Intern,
    ir::lower::{def::DefProc, loc::*},
};

macro_rules! new_ids {
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

new_ids! {
    Proc DefProc proc "Newtype of interned ID for procedure",
    Access AbsAccess access "Newtype of interned ID for path",
    Module ModuleLoc module_loc "Newtype of interned ID for module token",
    Crate CrateLoc crate_loc "Newtype of interned ID for crate token",
}
