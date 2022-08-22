//! Jar ingredients

use salsa::DebugWithDb;

use crate::base::{self, ln::LineTable};

#[salsa::tracked(return_ref, jar = base::Jar)]
pub fn line_table(db: &dyn base::Db, input_file: InputFile) -> LineTable {
    let source_text = input_file.source_text(db);
    LineTable::new_raw(source_text)
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


