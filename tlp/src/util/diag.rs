//! Diagnostic rendering

use std::fmt;

use base::{jar::InputFile, span::Span};
use colored::Colorize;

use crate::ir::IrDb;

pub const QUOTE: colored::Color = colored::Color::BrightBlue;

pub trait Diagnostic {
    // [<code]<severity>: msg
    fn code(&self) -> &str;
    fn severity(&self) -> Severity;
    fn msg(&self) -> &str;
    fn reason(&self) -> &str;
    //    ^^^^ <reason>
}

/// Line diagnostic
pub fn line<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    span: Span,
) -> Line<'a> {
    let src_file = input_file.name(db.base()).as_str(db.base());

    let ln_tbl = input_file.line_column_table(db.base());

    let ln_col = ln_tbl.line_column(span.start);
    let line = ln_col.line1() as usize;
    let column = ln_col.column1() as usize;

    let src = input_file.source_text(db.base());
    let line_text_span = ln_tbl.line_span(span.start);
    // trim newline
    let line_text = line_text_span.slice(&src).trim_end();
    let line_span = span - line_text_span.start;

    Line {
        code: diag.code(),
        severity: diag.severity(),
        msg: diag.msg(),
        src_file,
        line,
        column,
        line_text,
        line_span,
        reason: diag.reason(),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warnign",
            Severity::Info => "info",
            Severity::Hint => "hint",
        }
    }

    pub fn color(&self) -> colored::Color {
        match self {
            Self::Error => colored::Color::Red,
            Self::Warning => colored::Color::Green,
            Self::Info => colored::Color::Blue,
            Self::Hint => colored::Color::Yellow,
        }
    }
}

pub struct Line<'a> {
    // <severity>[code]: <msg>
    pub code: &'a str,
    pub severity: Severity,
    pub msg: &'a str,
    // --> <src_file>
    //            |
    // <ln>:<col> | <line_text>
    //            |    ^^^^ <reason>
    pub src_file: &'a str,
    pub line: usize,
    pub column: usize,
    pub line_text: &'a str,
    pub line_span: Span,
    pub reason: &'a str,
}

impl<'a> fmt::Display for Line<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code = format!("{}[{}]", self.severity.as_str(), self.code);
        writeln!(
            f,
            "{}: {}",
            code.color(self.severity.color()).bold(),
            self.msg.bold()
        )?;
        writeln!(f, "  --> {}:{}:{}", self.src_file, self.line, self.column)?;

        let n_digits = self::n_digits(self.line);
        let indent = " ".repeat(n_digits);

        let vbar = "|".color(QUOTE).bold();
        let line = format!("{}", self.line);

        writeln!(f, "{indent} {vbar}")?;
        writeln!(f, "{} {vbar} {}", line.color(QUOTE).bold(), self.line_text)?;
        writeln!(
            f,
            "{indent} {vbar} {} {}",
            self.reason_range_string()
                .color(self.severity.color())
                .bold(),
            self.reason.color(self.severity.color()).bold(),
        )?;

        Ok(())
    }
}

impl<'a> Line<'a> {
    fn reason_range_string(&self) -> String {
        let mut s = String::new();

        // TODO: more efficiently
        s.push_str(&" ".repeat(self.line_span.start.into_usize()));
        s.push_str(&"^".repeat(self.line_span.len() as usize));

        s
    }
}

fn n_digits(mut x: usize) -> usize {
    let mut n_digits = 0;

    loop {
        x /= 10;
        n_digits += 1;

        if x == 0 {
            break;
        }
    }

    n_digits
}
