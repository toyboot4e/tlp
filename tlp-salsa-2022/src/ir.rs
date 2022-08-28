//! Intermediate representation

pub mod body;
pub mod item;
pub mod jar;

#[salsa::jar(db = Db)]
pub struct Jar(jar::ParsedFile, item::Proc);

pub trait Db: salsa::DbWithJar<Jar> + crate::base::Db {
    fn as_ir_db(&self) -> &dyn Db;
}

impl<T: salsa::DbWithJar<Jar> + crate::base::Db> Db for T {
    fn as_ir_db(&self) -> &dyn Db {
        self
    }
}
