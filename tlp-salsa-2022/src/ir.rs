//! Intermediate representation

pub mod item;
pub mod jar;

use crate::{base, syntax::ast};

#[salsa::jar(db = IrDb)]
pub struct Jar(parse, jar::ParsedFile, item::Proc);

pub trait IrDb: salsa::DbWithJar<Jar> + crate::base::BaseDb {
    fn as_ir_db(&self) -> &dyn IrDb;
}

impl<T: salsa::DbWithJar<Jar> + crate::base::BaseDb> IrDb for T {
    fn as_ir_db(&self) -> &dyn IrDb {
        self
    }
}

#[salsa::tracked(return_ref, jar = Jar)]
pub fn parse(db: &dyn IrDb, file: base::jar::InputFile) -> jar::ParsedFile {
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
