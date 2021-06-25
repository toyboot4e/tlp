/*!
Span of UTF-8 source text
*/

pub type TextLen = usize;
pub type TextPos = usize;

/// Span of source text in range `(lo, hi]` referred to as `sp`
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ByteSpan {
    pub lo: TextPos,
    pub hi: TextPos,
}

impl ByteSpan {
    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        &src[self.lo..self.hi]
    }

    pub fn len(&self) -> TextLen {
        self.hi - self.lo
    }
}
