/*!
Byte-based span of source text
*/

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ByteSpan {
    pub lo: usize,
    pub hi: usize,
}

impl ByteSpan {
    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        &src[self.lo..self.hi]
    }
}
