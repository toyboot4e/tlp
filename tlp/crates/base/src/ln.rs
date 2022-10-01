//! Line, column and byte code offset conversion

use crate::{
    jar::{self, InputFile},
    span::{LineColumn, Offset, Span},
};

/// Converts a character index `position` into a line and column tuple.
pub fn line_column(db: &dyn crate::BaseDb, input_file: InputFile, position: Offset) -> LineColumn {
    let table = jar::line_table(db, input_file);
    table.line_column(position)
}

/// Given a line/column tuple, returns a character index.
pub fn offset(db: &dyn crate::BaseDb, input_file: InputFile, position: LineColumn) -> Offset {
    let table = jar::line_table(db, input_file);
    table.offset(position)
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LineTable {
    /// Always has at least one element for the first line
    lines: Vec<LineInfo>,
    end_offset: Offset,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct LineInfo {
    /// Offset of line start
    start: Offset,
    /// Spans of chars with utf8 length > 1
    wide_chars: Vec<Span>,
}

impl LineTable {
    pub(crate) fn new_raw(source_text: &str) -> Self {
        let mut table = LineTable {
            lines: vec![LineInfo {
                start: Offset::from(0u32),
                wide_chars: Vec::new(),
            }],
            end_offset: Offset::from(source_text.len()),
        };
        for (i, c) in source_text.char_indices() {
            if c == '\n' {
                table.lines.push(LineInfo {
                    start: Offset::from(i + 1),
                    wide_chars: Vec::new(),
                })
            } else if c.len_utf8() > 1 {
                table.lines.last_mut().unwrap().wide_chars.push(Span {
                    start: Offset::from(i),
                    end: Offset::from(i + c.len_utf8()),
                });
            }
        }
        table
    }

    pub fn num_lines(&self) -> usize {
        self.lines.len()
    }

    pub fn offset(&self, position: LineColumn) -> Offset {
        if position.line0_usize() >= self.num_lines() {
            return self.end_offset;
        }
        let line = &self.lines[position.line0_usize()];
        let mut offset = u32::from(line.start + position.column0());
        for wc in line.wide_chars.iter() {
            if u32::from(wc.start) < offset {
                offset += wc.len() - 1;
            }
        }
        Offset::from(offset).min(self.end_offset)
    }

    pub fn line_column(&self, position: Offset) -> LineColumn {
        match self.lines.binary_search_by_key(&position, |l| l.start) {
            Ok(line0) => LineColumn::new0(line0, 0u32),
            Err(next_line0) => {
                let line0 = next_line0 - 1;
                let line = &self.lines[line0];
                // not quite column yet, because there may be wide characters between line start and position
                // at this point it's the byte offset from line start
                // we need to adjust for it
                let mut column0 = position - line.start;
                for wc in line.wide_chars.iter() {
                    if wc.start >= position {
                        break;
                    }
                    // e.g.: 🙂 will have len 4, but we count it as 1 character, so we subtract 3
                    column0 -= wc.len() - 1;
                }
                LineColumn::new0(line0, column0)
            }
        }
    }

    pub fn line_column_span(&self, span: Span) -> (LineColumn, LineColumn) {
        (self.line_column(span.start), self.line_column(span.end))
    }

    pub fn line_span(&self, position: Offset) -> Span {
        let (i, start) = match self.lines.binary_search_by_key(&position, |l| l.start) {
            Ok(line0) => (line0, self.lines[line0].start),
            Err(next_line0) => (next_line0 - 1, self.lines[next_line0 - 1].start),
        };

        let end = match self.lines.get(i + 1) {
            Some(line0) => line0.start,
            None => self.end_offset,
        };

        Span { start, end }
    }
}
