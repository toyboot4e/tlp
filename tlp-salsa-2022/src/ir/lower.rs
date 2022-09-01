//! Lowers code block into [`Body`]

use base::{
    jar::InputFile,
    span::Span,
    tbl::{origin_table::PushOriginIn, InternValue},
};

use crate::{
    ir::{
        body::{self, Expr, ExprData},
        item,
        jar::ParsedFile,
        IrDb, IrJar,
    },
    syntax::ast,
};

#[salsa::tracked(return_ref, jar = IrJar)]
pub(crate) fn lower_items(db: &dyn IrDb, file: base::jar::InputFile) -> ParsedFile {
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

// TODO: why not `return_ref`?
#[salsa::tracked(jar = IrJar)]
pub fn lower_body(db: &dyn IrDb, proc: item::Proc) -> body::Body {
    let input_file = proc.span(db).input_file;

    let mut lower = LowerBody {
        db,
        input_file,
        tables: Default::default(),
        spans: Default::default(),
        root_block: Default::default(),
    };

    lower.lower_root_proc(proc);
    lower.into_body()
}

struct LowerBody<'a> {
    db: &'a dyn IrDb,
    input_file: InputFile,
    tables: body::BodyTables,
    spans: body::BodySpans,
    root_block: Option<Expr>,
}

impl<'a> LowerBody<'a> {
    pub fn into_body(self) -> body::Body {
        let data = body::BodyData {
            tables: self.tables,
            root_block: self.root_block.unwrap(),
        };

        body::Body::new(self.db, data, self.spans)
    }

    pub fn lower_root_proc(&mut self, proc: item::Proc) {
        let ast = proc.ast(self.db);
        let ast_block = ast.block();

        let block = self.lower_block(ast_block);
        self.root_block = Some(block);
    }

    fn alloc<D, K>(&mut self, data: D, span: Span) -> K
    where
        D: std::hash::Hash + Eq + std::fmt::Debug,
        D: InternValue<Table = body::BodyTables, Key = K>,
        D: InternValue<Table = body::BodyTables, Key = K>,
        K: PushOriginIn<body::BodySpans, Origin = Span> + salsa::AsId,
    {
        let key = self.tables.add(data);
        self.spans.push(key, span);
        key
    }
}

impl<'a> LowerBody<'a> {
    fn lower_block(&mut self, ast_block: ast::Block) -> Expr {
        let block_data = ExprData::Block(Vec::new());
        let span = Span::from_rowan_range(ast_block.syn.text_range());
        let block = self.alloc(block_data, span);

        // TODO: loop

        block
    }
}
