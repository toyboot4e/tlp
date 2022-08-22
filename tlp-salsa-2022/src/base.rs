//! Base source handling taken from `dada` langauge code base

pub mod jar;
pub mod ln;
pub mod span;

#[salsa::jar(db = Db)]
pub struct Jar(jar::Word, jar::InputFile, jar::line_table);

pub trait Db: salsa::DbWithJar<Jar> {
    fn as_dyn_base_db(&self) -> &dyn Db;
}

impl<T: salsa::DbWithJar<Jar>> Db for T {
    fn as_dyn_base_db(&self) -> &dyn Db {
        self
    }
}
