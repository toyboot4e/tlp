/*!
Span of UTF-8 source text
*/

pub type TextLen = usize;
pub type TextPos = usize;
pub type LineNum = usize;
pub type Col = usize;

/// Span of source text in range `(lo, hi]` referred to as `sp`
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ByteSpan {
    pub lo: TextPos,
    pub hi: TextPos,
}

impl ByteSpan {
    /// One-byte span
    pub fn at(pos: TextPos) -> Self {
        Self {
            lo: pos,
            hi: pos + 1,
        }
    }

    pub fn slice<'a>(&self, src: &'a str) -> &'a str {
        &src[self.lo..self.hi]
    }

    pub fn len(&self) -> TextLen {
        self.hi - self.lo
    }
}

/// TODO: Support UTF-16
pub fn loc_to_pos(ln: LineNum, col: Col, src: &str) -> Option<TextPos> {
    let line = self::line_starts(src).nth(ln)?;
    Some(line + col)
}

/// TODO: Support UTF-16
pub fn locate(pos: TextPos, src: &str) -> (LineNum, Col) {
    let (nth_line, line_offset) = self::line(src, pos);

    let col = if line_offset >= src.len() {
        0
    } else {
        let hi = usize::min(pos, src.len());
        let s = &src[line_offset..hi];
        s.chars().count()
    };

    (nth_line, col)
}

/// Returns the line given `pos` is at with byte offset (line, line_offset)
///
/// NOTE: The `line_offset` can be `source_text.len() + 1`.
fn line<'src>(source: &'src str, pos: usize) -> (usize, usize) {
    let line_starts = self::line_starts(source).collect::<Vec<_>>();

    let nth_line = match line_starts.binary_search(&pos) {
        Ok(line) => line,
        Err(next_line) => next_line - 1,
    };
    (nth_line, line_starts[nth_line])
}

fn line_starts<'src>(source: &'src str) -> impl 'src + Iterator<Item = usize> {
    std::iter::once(0).chain(source.match_indices('\n').map(|(i, _pat)| i + 1))
}
