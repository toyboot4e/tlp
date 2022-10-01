//! Diagnostic rendering

use std::fmt;

use base::{
    jar::InputFile,
    span::{LineColumn, Span},
};
use colored::Colorize;

use crate::ir::IrDb;

pub const QUOTE: colored::Color = colored::Color::BrightBlue;

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

pub trait Diagnostic {
    // [<code]<severity>: msg
    fn code(&self) -> &str;
    fn severity(&self) -> Severity;
    fn msg(&self) -> &str;
}

/// ```text
/// <severity>[code]: <msg>
/// --> <src_file>:<ln>:<col>
/// ```
#[derive(Debug)]
pub struct Header<'a> {
    pub code: &'a str,
    pub severity: Severity,
    pub msg: &'a str,
    pub src_file: &'a str,
    pub ln_col: LineColumn,
}

impl<'a> Header<'a> {
    pub fn new(diag: &'a impl Diagnostic, src_file: &'a str, ln_col: LineColumn) -> Self {
        Self {
            code: diag.code(),
            severity: diag.severity(),
            msg: diag.msg(),
            src_file,
            ln_col,
        }
    }
}

impl<'a> fmt::Display for Header<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let severity_code = format!("{}[{}]", self.severity.as_str(), self.code);

        writeln!(
            f,
            "{}: {}\n--> {}:{}:{}",
            severity_code.color(self.severity.color()).bold(),
            self.msg.bold(),
            self.src_file,
            self.ln_col.line1(),
            self.ln_col.column1(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MsgSpan<'a> {
    /// Span in the source text
    pub span: Span,
    /// Message for the spanned text
    pub msg: &'a str,
}

impl<'a> MsgSpan<'a> {
    pub fn new(line_span: Span, msg: &'a str) -> Self {
        Self {
            span: line_span,
            msg,
        }
    }
}

/// ```text
///      |
/// <ln> | <line_text>
///      |    ^^^^ <msg>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LineSpanMain<'a> {
    pub severity: Severity,
    /// Line number for merge and display
    pub line1: u32,
    pub src_text: &'a str,
    pub line_span: Span,
    pub main: MsgSpan<'a>,
}

impl<'a> fmt::Display for LineSpanMain<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n_digits = self::n_digits(self.line1 as usize);
        let indent = " ".repeat(n_digits);

        let vbar = "|".color(QUOTE).bold();
        let line = format!("{}", self.line1);
        let line_text = self.line_span.slice(self.src_text).trim_end();

        writeln!(f, "{indent} {vbar}")?;
        writeln!(f, "{} {vbar} {}", line.color(QUOTE).bold(), line_text)?;
        writeln!(
            f,
            "{indent} {vbar} {} {}",
            self::msg_range_string(self.main.span - self.line_span.start)
                .color(self.severity.color())
                .bold(),
            self.main.msg.color(self.severity.color()).bold(),
        )?;

        Ok(())
    }
}

/// ```text
///      |
/// <ln> | <line_text>
///      |  ^ -- ---- expected `f32`, found `i32`
///      |    |
///      |    expected `u32`, found `i32`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LineSpanSubs<'a> {
    /// Line number for merge and display
    pub line1: usize,
    /// Absolute span for the line
    pub line_span: Span,
    pub main: MsgSpan<'a>,
    pub subs: Vec<MsgSpan<'a>>,
}

/// Diagnostic window
#[derive(Debug)]
pub enum Window<'a> {
    Main(LineSpanMain<'a>),
    Subs(LineSpanSubs<'a>),
}

impl<'a> fmt::Display for Window<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Main(x) => x.fmt(f),
            Self::Subs(x) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct Render<'a> {
    pub header: Header<'a>,
    pub window: Window<'a>,
}

impl<'a> fmt::Display for Render<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: each format contains the last newline character, so don't use `writeln!`
        write!(f, "{}", self.header)?;
        write!(f, "{}", self.window)?;
        Ok(())
    }
}

/// Line diagnostic window
pub fn line<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    main: MsgSpan<'a>,
) -> Render<'a> {
    let src_file = input_file.name(db.base()).as_str(db.base());
    let src_text = input_file.source_text(db.base());

    let ln_tbl = input_file.line_column_table(db.base());
    let ln_col = ln_tbl.line_column(main.span.start);

    let header = Header::new(diag, src_file, ln_col);

    let line_span = ln_tbl.line_span(main.span.start);

    let main_span = LineSpanMain {
        severity: diag.severity(),
        line1: ln_col.line1(),
        src_text,
        line_span,
        main,
    };

    Render {
        header,
        window: Window::Main(main_span),
    }
}

pub fn multi_subs<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    main_span: Span,
    subs: Vec<MsgSpan>,
) -> Render<'a> {
    todo!()
}

/// Uses span relative to the line start
fn msg_range_string(line_span: Span) -> String {
    let mut s = String::new();

    // TODO: more efficiently
    s.push_str(&" ".repeat(line_span.start.into_usize()));
    s.push_str(&"^".repeat(line_span.len() as usize));

    s
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
