//! Item

use salsa::DebugWithDb;

use base::{
    jar::{InputFile, SpannedWord, Word},
    span::*,
};

use crate::ir::IrDb;

use crate::{
    ir::{
        self,
        body::{expr, pat},
    },
    syntax::ast,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Item {
    Proc(Proc),
    // TODO: Import(Import),
}

impl Item {
    pub fn debug<'a>(&'a self, db: &'a dyn ir::IrDb) -> Box<dyn std::fmt::Debug + 'a> {
        match self {
            Item::Proc(p) => Box::new(p.debug(db)),
        }
    }
}

impl Item {
    pub fn span(self, db: &dyn ir::IrDb) -> FileSpan {
        match self {
            Item::Proc(f) => f.span(db),
        }
    }

    pub fn name(self, db: &dyn ir::IrDb) -> Word {
        match self {
            Item::Proc(f) => f.name(db).word(db.base()),
        }
    }

    pub fn name_span(self, db: &dyn ir::IrDb) -> FileSpan {
        match self {
            Item::Proc(f) => f.name(db).span(db.base()),
        }
    }

    pub fn kind_str(self) -> &'static str {
        match self {
            Item::Proc(_) => "procedure",
        }
    }
}

impl From<Proc> for Item {
    fn from(value: Proc) -> Self {
        Self::Proc(value)
    }
}

#[salsa::tracked(jar = ir::IrJar)]
pub struct Proc {
    /// Name used as `#[id]`
    // TODO: measure recomputation and consider separating the span from word
    #[id]
    pub name: SpannedWord,
    pub span: FileSpan,
    #[return_ref]
    pub params: Box<[Param]>,
    // #[return_ref]
    // pub return_ty: expr::Type,
    /// Lazily analyzed body AST
    pub ast: ast::DefProc,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: pat::PatData,
    pub ty: expr::TypeSyntax,
    // pub span: FileSpan,
}

impl Param {
    pub fn from_ast(db: &dyn ir::IrDb, ast: ast::Param) -> Option<Self> {
        let pat = ast.pat().and_then(|pat| pat::PatData::from_ast(db, pat))?;

        let ty = expr::TypeSyntax::from_opt_ast(db, ast.ty());
        Some(Param { pat, ty })
    }
}

impl Proc {
    pub fn input_file(&self, db: &dyn IrDb) -> InputFile {
        self.span(db).input_file
    }

    pub fn from_ast(
        db: &dyn ir::IrDb,
        file: base::jar::InputFile,
        ast: ast::DefProc,
    ) -> Option<Self> {
        let name = {
            // here we require function name for simplicity
            let name_tk = ast.name()?;
            let name = Word::intern(db.base(), name_tk.token().text());

            let name_span = Span::from_rowan_range(name_tk.syn.text_range());
            let name_span = FileSpan {
                input_file: file,
                start: name_span.start,
                end: name_span.end,
            };

            SpannedWord::new(db.base(), name, name_span)
        };

        let file_span = Span::from_rowan_range(ast.syn.text_range());
        let file_span = FileSpan {
            input_file: file,
            start: file_span.start,
            end: file_span.end,
        };

        let mut params = Vec::new();

        if let Some(params_node) = ast.params() {
            for param_node in params_node.nodes() {
                if let Some(param) = Param::from_ast(db, param_node) {
                    params.push(param);
                } else {
                    // TODO: consider still adding parameter?
                }
            }
        }

        Some(Proc::new(
            db,
            name,
            file_span,
            params.into_boxed_slice(),
            ast,
        ))
    }
}
