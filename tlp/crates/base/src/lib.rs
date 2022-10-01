//! Source handling
//!
//! Taken from the `dada` langauge code base.

pub mod jar;
pub mod ln;
pub mod span;
pub mod tbl;

#[salsa::jar(db = BaseDb)]
pub struct BaseJar(
    jar::InputFile,
    jar::line_table,
    jar::Word,
    jar::SpannedWord,
    jar::SpannedOptionalWord,
);

pub trait BaseDb: salsa::DbWithJar<BaseJar> {
    fn base(&self) -> &dyn BaseDb;
}

impl<T: salsa::DbWithJar<BaseJar>> BaseDb for T {
    fn base(&self) -> &dyn BaseDb {
        self
    }
}

impl jar::InputFile {
    pub fn line_column_table<'db>(&self, db: &'db dyn BaseDb) -> &'db ln::LineTable {
        jar::line_table(db, *self)
    }

    /// Converts a given offset in a given file into line/column information.
    pub fn line_column_at(&self, db: &dyn BaseDb, offset: span::Offset) -> span::LineColumn {
        ln::line_column(db, *self, offset)
    }
}

impl span::FileSpan {
    /// Converts a `FileSpan` into its constituent parts.
    pub fn line_column_spans(
        &self,
        db: &dyn BaseDb,
    ) -> (jar::InputFile, span::LineColumn, span::LineColumn) {
        let start = self.input_file.line_column_at(db, self.start);
        let end = self.input_file.line_column_at(db, self.end);
        (self.input_file, start, end)
    }
}

// TODO: remote those interpolation code to the `syntax` module

impl span::Span {
    pub fn from_rowan_range(range: rowan::TextRange) -> Self {
        let (start, end): (u32, u32) = (range.start().into(), range.end().into());
        Self::from(start, end)
    }

    pub fn into_rowan_rang(self) -> rowan::TextRange {
        let (start, end): (u32, u32) = (self.start.into(), self.end.into());
        rowan::TextRange::new(start.into(), end.into())
    }

    pub fn slice<'a>(&self, s: &'a str) -> &'a str {
        &s[self.start.into_usize()..self.end.into_usize()]
    }
}
