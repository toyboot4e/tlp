//! Base source handling taken from the `dada` langauge code base

pub mod jar;
pub mod ln;
pub mod span;

// TODO: include this?
// pub mod tbl;

#[salsa::jar(db = Db)]
pub struct Jar(
    jar::InputFile,
    jar::line_table,
    jar::Word,
    jar::SpannedWord,
    jar::SpannedOptionalWord,
);

pub trait Db: salsa::DbWithJar<Jar> {
    fn as_base_db(&self) -> &dyn Db;
}

impl<T: salsa::DbWithJar<Jar>> Db for T {
    fn as_base_db(&self) -> &dyn Db {
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
