//! Jar ingredients

use salsa::DebugWithDb;

use crate::base::{self, ln::LineTable, span::FileSpan};

#[salsa::tracked(return_ref, jar = base::Jar)]
pub fn line_table(db: &dyn base::Db, input_file: InputFile) -> LineTable {
    let source_text = input_file.source_text(db);
    LineTable::new_raw(source_text)
}

#[salsa::input(jar = base::Jar)]
pub struct InputFile {
    name: Word,
    #[return_ref]
    source_text: String,
}

impl InputFile {
    pub fn name_str(self, db: &dyn base::Db) -> &str {
        self.name(db).string(db)
    }
}

impl DebugWithDb<dyn base::Db + '_> for InputFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn base::Db) -> std::fmt::Result {
        f.debug_tuple("SourceFile")
            .field(&self.name(db).debug(db))
            .finish()
    }
}

#[salsa::interned(jar = base::Jar)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Word {
    #[return_ref]
    pub string: String,
}

impl Word {
    pub fn intern(db: &dyn base::Db, string: impl ToString) -> Self {
        Word::new(db, string.to_string())
    }

    pub fn as_str(self, db: &dyn base::Db) -> &str {
        self.string(db)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(self, db: &dyn base::Db) -> u32 {
        self.as_str(db).len() as u32
    }
}

impl salsa::DebugWithDb<dyn base::Db + '_> for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn base::Db) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.string(db), f)
    }
}

/// A "spanned word" is a `Word` that also carries a span. Useful for things like
/// argument names etc where we want to carry the span through many phases
/// of compilation.
#[salsa::tracked(jar = base::Jar)]
pub struct SpannedWord {
    #[id]
    word: Word,
    span: FileSpan,
}

impl SpannedWord {
    pub fn as_str(self, db: &dyn base::Db) -> &str {
        self.word(db).as_str(db)
    }
}

impl<Db: ?Sized + base::Db> salsa::DebugWithDb<Db> for SpannedWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.as_str(db.as_base_db()), f)
    }
}

/// An optional SpannedOptionalWord is an identifier that may not be persent; it still carries
/// a span for where the label *would have gone* had it been present (as compared to
/// an `Option<Label>`).
#[salsa::tracked(jar = base::Jar)]
pub struct SpannedOptionalWord {
    #[id]
    word: Option<Word>,
    span: FileSpan,
}

impl SpannedOptionalWord {
    pub fn as_str(self, db: &dyn base::Db) -> Option<&str> {
        Some(self.word(db)?.as_str(db))
    }
}

impl<Db: ?Sized + base::Db> salsa::DebugWithDb<Db> for SpannedOptionalWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.as_str(db.as_base_db()), f)
    }
}
