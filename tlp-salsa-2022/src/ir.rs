//! Intermediate representation

pub mod body;
pub mod item;
pub mod jar;
pub mod lower;

#[salsa::jar(db = IrDb)]
pub struct IrJar(
    jar::ParsedFile,
    lower::lower_items,
    lower::lower_body,
    item::Proc,
    body::Body,
);

pub trait IrDb: salsa::DbWithJar<IrJar> + base::BaseDb {
    fn as_ir_db(&self) -> &dyn IrDb;
}

impl<T: salsa::DbWithJar<IrJar> + base::BaseDb> IrDb for T {
    fn as_ir_db(&self) -> &dyn IrDb {
        self
    }
}

/// Extensions
pub trait InputFileExt {
    fn source_file(self, db: &dyn IrDb) -> &jar::ParsedFile;
    fn items(self, db: &dyn IrDb) -> &[item::Item];
}

/// Extensions
impl InputFileExt for base::jar::InputFile {
    fn source_file(self, db: &dyn IrDb) -> &jar::ParsedFile {
        lower::lower_items(db, self)
    }

    fn items(self, db: &dyn IrDb) -> &[item::Item] {
        lower::lower_items(db, self).items(db)
    }
}

impl item::Proc {
    pub fn body(&self, db: &dyn IrDb) -> body::Body {
        lower::lower_body(db, *self)
    }
}
