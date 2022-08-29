//! Source handling
//!
//! Taken from the `dada` langauge code base.

pub mod jar;
pub mod ln;
pub mod span;

// TODO: include this?
// pub mod tbl;

#[salsa::jar(db = BaseDb)]
pub struct BaseJar(
    jar::InputFile,
    jar::line_table,
    jar::Word,
    jar::SpannedWord,
    jar::SpannedOptionalWord,
);

pub trait BaseDb: salsa::DbWithJar<BaseJar> {
    fn as_base_db(&self) -> &dyn BaseDb;
}

impl<T: salsa::DbWithJar<BaseJar>> BaseDb for T {
    fn as_base_db(&self) -> &dyn BaseDb {
        self
    }
}

// TODO: remote those interpolation code to the `syntax` module

impl span::Span {
    pub fn from_rowan_range(rng: rowan::TextRange) -> Self {
        let (start, end): (u32, u32) = (rng.start().into(), rng.end().into());
        Self::from(start, end)
    }

    pub fn slice<'a>(&self, s: &'a str) -> &'a str {
        &s[self.start.into_usize()..self.end.into_usize()]
    }
}
