//! Intermediate representation

pub mod item;
pub mod jar;
pub mod body;

use crate::syntax::ast;

#[salsa::jar(db = IrDb)]
pub struct IrJar(parse, jar::ParsedFile, item::Proc, body::Body);

pub trait IrDb: salsa::DbWithJar<IrJar> + base::BaseDb {
    fn as_ir_db(&self) -> &dyn IrDb;
}

impl<T: salsa::DbWithJar<IrJar> + base::BaseDb> IrDb for T {
    fn as_ir_db(&self) -> &dyn IrDb {
        self
    }
}

#[salsa::tracked(return_ref, jar = IrJar)]
fn parse(db: &dyn IrDb, file: base::jar::InputFile) -> jar::ParsedFile {
    let mut items = Vec::new();

    let src = file.source_text(db.as_base_db());
    let parse = ast::parse(src);
    let ast = parse.doc;

    for item in ast.items() {
        let item = match item {
            ast::Item::DefProc(ast) => {
                let proc = match item::Proc::from_ast(db, file, ast) {
                    Some(x) => x,
                    None => continue,
                };

                item::Item::Proc(proc)
            }
        };

        items.push(item);
    }

    jar::ParsedFile::new(db, file, items)
}

/// Extensions
pub trait InputFileExt {
    fn source_file(self, db: &dyn IrDb) -> &jar::ParsedFile;
    fn items(self, db: &dyn IrDb) -> &[item::Item];
}

/// Extensions
impl InputFileExt for base::jar::InputFile {
    fn source_file(self, db: &dyn IrDb) -> &jar::ParsedFile {
        self::parse(db, self)
    }

    fn items(self, db: &dyn IrDb) -> &[item::Item] {
        self::parse(db, self).items(db)
    }
}

