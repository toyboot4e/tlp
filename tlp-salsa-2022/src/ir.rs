//! Intermediate representation

pub mod body;
pub mod item;
pub mod jar;

#[salsa::jar(db = IrDb)]
pub struct Jar(jar::ParsedFile, item::Proc);

pub trait IrDb: salsa::DbWithJar<Jar> + crate::base::BaseDb {
    fn as_ir_db(&self) -> &dyn IrDb;
}

impl<T: salsa::DbWithJar<Jar> + crate::base::BaseDb> IrDb for T {
    fn as_ir_db(&self) -> &dyn IrDb {
        self
    }
}
