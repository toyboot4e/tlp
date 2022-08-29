//! Item

use crate::{
    base::{
        self,
        jar::{SpannedWord, Word},
        span::*,
    },
    ir,
    syntax::ast,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Item {
    Proc(Proc),
}

impl Item {
    pub fn span(self, db: &dyn ir::IrDb) -> FileSpan {
        match self {
            Item::Proc(f) => f.span(db),
        }
    }

    pub fn name(self, db: &dyn ir::IrDb) -> Word {
        match self {
            Item::Proc(f) => f.name(db).word(db.as_base_db()),
        }
    }

    pub fn name_span(self, db: &dyn ir::IrDb) -> FileSpan {
        match self {
            Item::Proc(f) => f.name(db).span(db.as_base_db()),
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

impl<Db: ?Sized + ir::IrDb> salsa::DebugWithDb<Db> for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        match self {
            Item::Proc(v) => std::fmt::Debug::fmt(&v.debug(db), f),
        }
    }
}

#[salsa::tracked(jar = ir::Jar)]
pub struct Proc {
    /// Name used as `#[id]`
    #[id]
    name: SpannedWord,
    span: FileSpan,
    /// Unparsed body code
    ast: ast::DefProc,
}

impl Proc {
    pub fn from_ast(
        db: &dyn ir::IrDb,
        file: base::jar::InputFile,
        ast: ast::DefProc,
    ) -> Option<Self> {
        let name = {
            // here we require function name for simplicity
            let name_tk = ast.name()?;
            let name = Word::intern(db.as_base_db(), name_tk.token().text());

            let name_span = Span::from_rowan_range(name_tk.syn.text_range());
            let name_span = FileSpan {
                input_file: file,
                start: name_span.start,
                end: name_span.end,
            };

            SpannedWord::new(db.as_base_db(), name, name_span)
        };

        let file_span = Span::from_rowan_range(ast.syn.text_range());
        let file_span = FileSpan {
            input_file: file,
            start: file_span.start,
            end: file_span.end,
        };

        Some(Proc::new(db, name, file_span, ast))
    }
}

impl<Db: ?Sized + ir::IrDb> salsa::DebugWithDb<Db> for Proc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        write!(f, "{}", self.name(db.as_ir_db()).as_str(db.as_base_db()))
    }
}
