//! Jar ingredients

use crate::{ln::LineTable, span::FileSpan};

#[salsa::tracked(return_ref, jar = crate::BaseJar)]
pub(crate) fn line_table(db: &dyn crate::BaseDb, input_file: InputFile) -> LineTable {
    let source_text = input_file.source_text(db);
    LineTable::new_raw(source_text)
}

#[salsa::input(jar = crate::BaseJar)]
pub struct InputFile {
    pub name: Word,
    #[return_ref]
    pub source_text: String,
}

impl InputFile {
    pub fn name_str(self, db: &dyn crate::BaseDb) -> &str {
        self.name(db).string(db)
    }
}

#[salsa::interned(jar = crate::BaseJar)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Word {
    #[return_ref]
    pub string: String,
}

impl Word {
    pub fn intern(db: &dyn crate::BaseDb, string: impl ToString) -> Self {
        Word::new(db, string.to_string())
    }

    pub fn as_str(self, db: &dyn crate::BaseDb) -> &str {
        self.string(db)
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(self, db: &dyn crate::BaseDb) -> u32 {
        self.as_str(db).len() as u32
    }
}

/// A "spanned word" is a `Word` that also carries a span. Useful for things like
/// argument names etc where we want to carry the span through many phases
/// of compilation.
#[salsa::tracked(jar = crate::BaseJar)]
pub struct SpannedWord {
    #[id]
    pub word: Word,
    pub span: FileSpan,
}

impl SpannedWord {
    pub fn as_str(self, db: &dyn crate::BaseDb) -> &str {
        self.word(db).as_str(db)
    }
}

/// An optional SpannedOptionalWord is an identifier that may not be persent; it still carries
/// a span for where the label *would have gone* had it been present (as compared to
/// an `Option<Label>`).
#[salsa::tracked(jar = crate::BaseJar)]
pub struct SpannedOptionalWord {
    #[id]
    pub word: Option<Word>,
    pub span: FileSpan,
}

impl SpannedOptionalWord {
    pub fn as_str(self, db: &dyn crate::BaseDb) -> Option<&str> {
        Some(self.word(db)?.as_str(db))
    }
}
