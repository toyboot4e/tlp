//! Diagnostic rendering

use std::fmt::{self, Write};

use base::{
    jar::InputFile,
    span::{LineColumn, Offset, Span},
};
use colored::{Color, Colorize};

use crate::ir::IrDb;

pub const QUOTE: Color = Color::BrightBlue;

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

    pub fn color(&self) -> Color {
        match self {
            Self::Error => Color::Red,
            Self::Warning => Color::Green,
            Self::Info => Color::Blue,
            Self::Hint => Color::Yellow,
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
pub struct MsgSpan {
    /// Span in the source text
    pub span: Span,
    /// Message for the spanned text
    pub msg: String,
}

impl MsgSpan {
    pub fn new(span: Span, msg: String) -> Self {
        Self { span, msg }
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
    pub main_msg: MsgSpan,
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
            self::msg_range_string(self.main_msg.span - self.line_span.start)
                .color(self.severity.color())
                .bold(),
            self.main_msg.msg.color(self.severity.color()).bold(),
        )?;

        Ok(())
    }
}

/// Spans for a line window
///
/// ```text
///      |
/// <ln> | <line_text>
///      |  ^ -- ---- expected `f32`, found `i32`
///      |    |
///      |    expected `u32`, found `i32`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LineSpanSubs<'a> {
    pub severity: Severity,
    /// Line number for merge and display
    pub line1: u32,
    pub src_text: &'a str,
    pub line_span: Span,
    pub main_span: Span,
    pub sub_msgs: Vec<MsgSpan>,
}

impl<'a> fmt::Display for LineSpanSubs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n_digits = self::n_digits(self.line1 as usize);
        let indent = " ".repeat(n_digits);

        let vbar = "|".color(QUOTE).bold();
        let line = format!("{}", self.line1);
        let line_text = self.line_span.slice(self.src_text).trim_end();

        writeln!(f, "{indent} {vbar}")?;
        writeln!(f, "{} {vbar} {}", line.color(QUOTE).bold(), line_text)?;

        // ranges + last sub message:
        let line_offset = self.line_span.start;
        {
            //  ^^^
            let last_relative_span = self.main_span - line_offset;

            let ws = " ".repeat(last_relative_span.start.into_usize());
            let carets = "^"
                .repeat(last_relative_span.len() as usize)
                .color(self.severity.color());
            write!(f, "{indent} {vbar} {ws}{}", carets)?;

            //        ---   ----
            self::print_markers(
                f,
                "-",
                line_offset,
                last_relative_span,
                self.sub_msgs.iter().map(|m| m.span),
            )?;

            //                   expected `f32`, found `i32`
            writeln!(
                f,
                " {}",
                self.sub_msgs
                    .last()
                    .unwrap()
                    .msg
                    .color(Color::BrightBlue)
                    .bold()
            )?;
        }

        let (vbars_string, vbars_spans) = self::format_bars(
            line_offset,
            self.sub_msgs[0..self.sub_msgs.len() - 1]
                .iter()
                .map(|m| m.span),
        )?;

        // sub messages in preceding lines:
        //    |  1.0  14
        // <1>   |    |
        // <2>   |    expected `f32`, found `i32`
        // <2>   expected because of this variable

        // <1>:  |    |
        writeln!(f, "{indent} {vbar} {vbars_string}")?;

        // <2>:  |    <msg>
        // <2>:  <msg>
        for i in (0..self.sub_msgs.len() - 1).rev() {
            let target_msg = &self.sub_msgs[i];

            let len = vbars_spans[i].end_ws.into_usize() - line_offset.into_usize();
            let vbars_slice = &vbars_string[0..len];

            writeln!(
                f,
                "{indent} {vbar} {vbars_slice}{}",
                target_msg.msg.color(Color::BrightBlue).bold()
            )?;
        }

        Ok(())
    }
}

///        ---   ----
fn print_markers(
    f: &mut fmt::Formatter<'_>,
    marker: &str,
    line_offset: Offset,
    mut last_relative_span: Span,
    spans: impl Iterator<Item = Span>,
) -> fmt::Result {
    for span in spans {
        let relative_span = span - line_offset;

        let ws = " ".repeat(relative_span.start.into_usize() - last_relative_span.end.into_usize());
        let markers = marker.repeat(relative_span.len() as usize);
        write!(f, "{ws}{}", markers.color(Color::BrightBlue).bold())?;

        last_relative_span = relative_span;
    }

    Ok(())
}

/// Span returned by [`format_bars`]
#[derive(Debug)]
struct VBarSpan {
    /// Span excluding `|`
    end_ws: Offset,
    /// Span including `|`
    end_vbar: Offset,
}

/// Formats colored vertical bars `  |  |` with slices
///
/// # Example
///
/// ```text
///   |  1.0  14
///      |    |
///      |    expected `f32`, found `i32`
///      expected because of this variable
/// ```
fn format_bars(
    line_offset: Offset,
    spans: impl Iterator<Item = Span>,
) -> Result<(String, Vec<VBarSpan>), fmt::Error> {
    let vbar = "|".color(Color::BrightBlue).bold();

    let mut s = String::new();
    let mut slices = Vec::new();

    // last span relative to the line offset
    let mut last_relative_span = Span {
        start: Offset::from(0u32),
        end: Offset::from(0u32),
    };

    for span in spans {
        let reletive_span = span - line_offset;
        let ws = " ".repeat(reletive_span.start.into_usize() - (last_relative_span.start.into_usize()));
        write!(s, "{ws}{vbar}")?;

        slices.push(VBarSpan {
            end_ws: Offset::from(span.end.into_usize() - vbar.len()),
            end_vbar: span.end,
        });

        last_relative_span = reletive_span;
    }

    Ok((s, slices))
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
            Self::Subs(x) => x.fmt(f),
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

pub fn header<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    main_span: Span,
) -> Header<'a> {
    let src_file = input_file.name(db.base()).as_str(db.base());
    let ln_tbl = input_file.line_column_table(db.base());
    let ln_col = ln_tbl.line_column(main_span.start);

    Header::new(diag, src_file, ln_col)
}

pub fn main_msg<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    main: MsgSpan,
) -> Render<'a> {
    let (header, line_span, src_text) = self::get(db, diag, input_file, main.span);

    let main_span = LineSpanMain {
        severity: diag.severity(),
        line1: header.ln_col.line1(),
        src_text,
        line_span,
        main_msg: main,
    };

    Render {
        header,
        window: Window::Main(main_span),
    }
}

pub fn sub_msgs<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    main_span: Span,
    sub_msgs: Vec<MsgSpan>,
) -> Render<'a> {
    let (header, line_span, src_text) = self::get(db, diag, input_file, main_span);

    let window = LineSpanSubs {
        severity: diag.severity(),
        line1: header.ln_col.line1(),
        src_text,
        line_span,
        main_span,
        sub_msgs,
    };

    Render {
        header,
        window: Window::Subs(window),
    }
}

fn get<'a>(
    db: &'a dyn IrDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    main_span: Span,
) -> (Header<'a>, Span, &'a str) {
    let src_file = input_file.name(db.base()).as_str(db.base());
    let ln_tbl = input_file.line_column_table(db.base());
    let ln_col = ln_tbl.line_column(main_span.start);
    let header = Header::new(diag, src_file, ln_col);

    let line_span = ln_tbl.line_span(main_span.start);
    let src_text = input_file.source_text(db.base());

    (header, line_span, src_text)
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
