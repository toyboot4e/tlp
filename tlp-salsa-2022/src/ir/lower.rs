//! Lowers code block into [`Body`]

use crate::{
    ir::{item, jar::ParsedFile, IrDb, IrJar},
    syntax::ast,
};

#[salsa::tracked(return_ref, jar = IrJar)]
pub(crate) fn parse(db: &dyn IrDb, file: base::jar::InputFile) -> ParsedFile {
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

    ParsedFile::new(db, file, items)
}
