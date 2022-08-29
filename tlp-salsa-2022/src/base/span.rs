//! Source span based on byte offset

use crate::base::{self, jar::InputFile, ln};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileSpan {
    pub input_file: InputFile,
    pub start: Offset,
    pub end: Offset,
}

impl FileSpan {
    pub fn snippet<'db>(&self, db: &'db dyn base::BaseDb) -> &'db str {
        &self.input_file.source_text(db)[usize::from(self.start)..usize::from(self.end)]
    }

    /// True if the given character falls within this span.
    pub fn contains(&self, offset: Offset) -> bool {
        self.start <= offset && offset < self.end
    }
}

impl<Db: ?Sized + base::BaseDb> salsa::DebugWithDb<Db> for FileSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let db = db.as_base_db();
        let start = ln::line_column(db, self.input_file, self.start);
        let end = ln::line_column(db, self.input_file, self.end);
        write!(
            f,
            "{}:{}:{}:{}:{}",
            self.input_file.name_str(db),
            start.line1(),
            start.column1(),
            end.line1(),
            end.column1(),
        )
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Offset,
    pub end: Offset,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}..{:?})", self.start.0, self.end.0)
    }
}

/// 0-based byte offset within a file.
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Offset(u32);

impl Offset {
    pub fn into_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LineColumn {
    /// 0-based line number
    line0: u32,

    /// 0-based column nuimber
    column0: u32,
}

impl LineColumn {
    /// Create from 1-based line/column numbers
    pub fn new1(line1: u32, column1: u32) -> Self {
        assert!(line1 > 0);
        assert!(column1 > 0);
        Self::new0(line1 - 1, column1 - 1)
    }

    /// Create from 0-based line/column numbers
    pub fn new0(line0: impl U32OrUsize, column0: impl U32OrUsize) -> Self {
        Self {
            line0: line0.into_u32(),
            column0: column0.into_u32(),
        }
    }

    pub fn line0(&self) -> u32 {
        self.line0
    }

    pub fn line1(&self) -> u32 {
        self.line0 + 1
    }

    pub fn line0_usize(&self) -> usize {
        usize::try_from(self.line0).unwrap()
    }

    pub fn column0(&self) -> u32 {
        self.column0
    }

    pub fn column1(&self) -> u32 {
        self.column0 + 1
    }
}

impl From<FileSpan> for Span {
    fn from(fs: FileSpan) -> Span {
        Span {
            start: fs.start,
            end: fs.end,
        }
    }
}

impl Span {
    #[track_caller]
    pub fn from(start: impl Into<Offset>, end: impl Into<Offset>) -> Self {
        let this = Self {
            start: start.into(),
            end: end.into(),
        };
        assert!(this.start <= this.end);
        this
    }

    pub fn in_file(self, input_file: InputFile) -> FileSpan {
        FileSpan {
            input_file,
            start: self.start,
            end: self.end,
        }
    }

    pub fn snippet<'db>(&self, db: &'db dyn base::BaseDb, input_file: InputFile) -> &'db str {
        self.in_file(input_file).snippet(db)
    }

    /// Returns a 0-length span at the start of this span
    #[must_use]
    pub fn span_at_start(self) -> Span {
        Span {
            start: self.start,
            end: self.start,
        }
    }

    pub fn zero() -> Self {
        Self {
            start: Offset(0),
            end: Offset(0),
        }
    }

    pub fn len(self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    #[must_use]
    pub fn to(self, other: Span) -> Span {
        assert!(self.start <= other.start && other.end >= self.end);
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

impl std::ops::Add<u32> for Offset {
    type Output = Offset;

    fn add(self, other: u32) -> Offset {
        Offset(self.0 + other)
    }
}

impl std::ops::AddAssign<u32> for Offset {
    fn add_assign(&mut self, other: u32) {
        *self = *self + other;
    }
}

impl std::ops::Add<usize> for Offset {
    type Output = Offset;

    fn add(self, other: usize) -> Offset {
        assert!(other < std::u32::MAX as usize);
        self + (other as u32)
    }
}

impl std::ops::AddAssign<usize> for Offset {
    fn add_assign(&mut self, other: usize) {
        *self = *self + other;
    }
}

impl std::ops::Sub<Offset> for Offset {
    type Output = u32;

    fn sub(self, other: Offset) -> u32 {
        self.0 - other.0
    }
}

impl From<usize> for Offset {
    fn from(value: usize) -> Offset {
        assert!(value < std::u32::MAX as usize);
        Offset(value as u32)
    }
}

impl From<u32> for Offset {
    fn from(value: u32) -> Offset {
        Offset(value)
    }
}

impl From<Offset> for u32 {
    fn from(offset: Offset) -> Self {
        offset.0
    }
}

impl From<Offset> for usize {
    fn from(offset: Offset) -> Self {
        offset.0 as usize
    }
}

pub trait U32OrUsize {
    fn into_u32(self) -> u32;
    fn from_u32(n: u32) -> Self;
    fn into_usize(self) -> usize;
    fn from_usize(n: usize) -> Self;
}

impl U32OrUsize for u32 {
    fn into_u32(self) -> u32 {
        self
    }

    fn from_u32(n: u32) -> Self {
        n
    }

    fn into_usize(self) -> usize {
        usize::try_from(self).unwrap()
    }

    fn from_usize(n: usize) -> Self {
        u32::try_from(n).unwrap()
    }
}

impl U32OrUsize for usize {
    fn into_u32(self) -> u32 {
        u32::try_from(self).unwrap()
    }

    fn from_u32(n: u32) -> Self {
        usize::try_from(n).unwrap()
    }

    fn into_usize(self) -> usize {
        self
    }

    fn from_usize(n: usize) -> Self {
        n
    }
}
