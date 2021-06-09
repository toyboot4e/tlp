/*!
Byte-based span of source text
*/

pub struct ByteSpan {
    pub lo: usize,
    pub hi: usize,
}

impl ByteSpan {
    pub fn slice(&self, src: &str) -> &str {
        src[self.lo..self.hi]
    }
}
