//! Item

use crate::{
    base::{
        jar::{SpannedWord, Word},
        span::*,
    },
    ir,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Item {
    Proc(Proc),
}

impl Item {
    pub fn span(self, db: &dyn ir::Db) -> FileSpan {
        match self {
            Item::Proc(f) => f.span(db),
        }
    }

    pub fn name(self, db: &dyn ir::Db) -> Word {
        match self {
            Item::Proc(f) => f.name(db).word(db.as_base_db()),
        }
    }

    pub fn name_span(self, db: &dyn ir::Db) -> FileSpan {
        match self {
            Item::Proc(f) => f.name(db).span(db.as_base_db()),
        }
    }

    pub fn kind_str(self) -> &'static str {
        match self {
            Item::Proc(_) => "function",
        }
    }
}

impl From<Proc> for Item {
    fn from(value: Proc) -> Self {
        Self::Proc(value)
    }
}

impl<Db: ?Sized + ir::Db> salsa::DebugWithDb<Db> for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        match self {
            Item::Proc(v) => std::fmt::Debug::fmt(&v.debug(db), f),
        }
    }
}

#[salsa::tracked(jar = ir::Jar)]
pub struct Proc {
    #[id]
    name: SpannedWord,
    span: FileSpan,
    // body: Option<Body>,
}

impl<Db: ?Sized + ir::Db> salsa::DebugWithDb<Db> for Proc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        write!(f, "{}", self.name(db.as_ir_db()).as_str(db.as_base_db()))
    }
}
