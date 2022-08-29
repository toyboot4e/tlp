//! Jar ingredients

use salsa::DebugWithDb;

use crate::base::{self, ln::LineTable, span::FileSpan};

#[salsa::tracked(return_ref, jar = base::BaseJar)]
pub fn line_table(db: &dyn base::BaseDb, input_file: InputFile) -> LineTable {
    let source_text = input_file.source_text(db);
    LineTable::new_raw(source_text)
}

#[salsa::input(jar = base::BaseJar)]
pub struct InputFile {
    name: Word,
    #[return_ref]
    source_text: String,
}

impl InputFile {
    pub fn name_str(self, db: &dyn base::BaseDb) -> &str {
        self.name(db).string(db)
    }
}

impl DebugWithDb<dyn base::BaseDb + '_> for InputFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn base::BaseDb) -> std::fmt::Result {
        f.debug_tuple("SourceFile")
            .field(&self.name(db).debug(db))
            .finish()
    }
}

#[salsa::interned(jar = base::BaseJar)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Word {
    #[return_ref]
    pub string: String,
}

impl Word {
    pub fn intern(db: &dyn base::BaseDb, string: impl ToString) -> Self {
        Word::new(db, string.to_string())
    }

    pub fn as_str(self, db: &dyn base::BaseDb) -> &str {
        self.string(db)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(self, db: &dyn base::BaseDb) -> u32 {
        self.as_str(db).len() as u32
    }
}

impl salsa::DebugWithDb<dyn base::BaseDb + '_> for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn base::BaseDb) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.string(db), f)
    }
}

/// A "spanned word" is a `Word` that also carries a span. Useful for things like
/// argument names etc where we want to carry the span through many phases
/// of compilation.
#[salsa::tracked(jar = base::BaseJar)]
pub struct SpannedWord {
    #[id]
    word: Word,
    span: FileSpan,
}

impl SpannedWord {
    pub fn as_str(self, db: &dyn base::BaseDb) -> &str {
        self.word(db).as_str(db)
    }
}

impl<Db: ?Sized + base::BaseDb> salsa::DebugWithDb<Db> for SpannedWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.as_str(db.as_base_db()), f)
    }
}

/// An optional SpannedOptionalWord is an identifier that may not be persent; it still carries
/// a span for where the label *would have gone* had it been present (as compared to
/// an `Option<Label>`).
#[salsa::tracked(jar = base::BaseJar)]
pub struct SpannedOptionalWord {
    #[id]
    word: Option<Word>,
    span: FileSpan,
}

impl SpannedOptionalWord {
    pub fn as_str(self, db: &dyn base::BaseDb) -> Option<&str> {
        Some(self.word(db)?.as_str(db))
    }
}

impl<Db: ?Sized + base::BaseDb> salsa::DebugWithDb<Db> for SpannedOptionalWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.as_str(db.as_base_db()), f)
    }
}
