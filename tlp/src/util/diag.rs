//! Diagnostic rendering

use std::fmt::{self, Write};

use base::{
    jar::InputFile,
    span::{LineColumn, Offset, Span},
    BaseDb,
};
use colored::{Color, Colorize};

const QUOTE: Color = Color::BrightBlue;

const R_ARROW: &'static str = "──►";

/// Horizontal bar string
const HBAR: &'static str = "─";

/// Vertical bar string
const VBAR: &'static str = "│";

const VDOT: &'static str = "┊";

/// Left up corner of rectangle
const RECT_LU: &'static str = "├";

/// Right up corner of rectangle
const RECT_RU: &'static str = "╯";
// const RECT_RU: &'static str = "┘";

/// Left down corner of rectangle
const RECT_LD: &'static str = "╰";
// const RECT_LD: &'static str = "└";

const INV_T: &'static str = "┴";

/// Secondary diagnostic messages under marked text span
const UNDER_MSG_PREFIX: &'static str = "╰──";

/// Diagnostic window rendering with a primary message only
pub fn render_single_msg<'a>(
    db: &'a dyn BaseDb,
    diag: &'a impl Diagnostic,
    input_file: InputFile,
    src_context: String,
    primary_msg: MsgSpan,
) -> Render<'a> {
    let (header, line_span, src_text) =
        self::get_header(db, diag, input_file, src_context, primary_msg.span);

    let primary_span = PrimaryLineRender {
        severity: diag.severity(),
        line1: header.ln_col.line1(),
        src_text,
        line_span,
        primary_msg,
    };

    Render {
        header,
        window: Window::Primary(primary_span),
        sub_renders: Vec::new(),
    }
}

/// Diagnostic window rendering with a primary span and secondary messages
pub fn render_multi_msg<'a>(
    db: &'a dyn BaseDb,
    diag: &impl Diagnostic,
    input_file: InputFile,
    src_context: String,
    primary_span: Span,
    secondary_msgs: Vec<MsgSpan>,
) -> Render<'a> {
    let (header, _line_span, _src_text) =
        self::get_header(db, diag, input_file, src_context, primary_span);

    let window = self::window_multi_msg(
        db,
        diag.severity(),
        input_file,
        primary_span,
        secondary_msgs,
    );

    Render {
        header,
        window,
        sub_renders: Vec::new(),
    }
}

/// Error | Warning | Note | Hint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
    Note,
    Hint,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SeverityDisplay {
    pub severity: Severity,
    pub code: Option<&'static str>,
}

impl fmt::Display for SeverityDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(code) = &self.code {
            let s = format!("[{}]{}", code, self.severity.as_str());
            write!(f, "{}", s.color(self.severity.color()).bold())
        } else {
            write!(
                f,
                "{}",
                self.severity.as_str().color(self.severity.color()).bold()
            )
        }
    }
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warnign",
            Severity::Note => "note",
            Severity::Hint => "hint",
        }
    }

    pub fn color(&self) -> Color {
        match self {
            Self::Error => Color::Red,
            Self::Warning => Color::Green,
            Self::Note => Color::Blue,
            Self::Hint => Color::Yellow,
        }
    }
}

/// Basic diagnostic information
pub trait Diagnostic {
    // [<code]<severity>: msg
    fn code(&self) -> Option<&'static str> {
        None
    }

    fn severity(&self) -> Severity;

    fn severity_display(&self) -> SeverityDisplay {
        SeverityDisplay {
            severity: self.severity(),
            code: self.code(),
        }
    }

    /// Primary error message shown in the header
    fn msg(&self) -> String;
}

/// Header part of diagnostic rendering
///
/// ```text
/// <severity>[code]: <msg>
/// --> <src_file>:<ln>:<col> <src_context>
/// ```
#[derive(Debug)]
pub struct Header<'a> {
    pub severity_display: SeverityDisplay,
    pub msg: String,
    pub src_file: &'a str,
    /// in procedure `f`
    pub src_context: String,
    pub ln_col: LineColumn,
}

impl<'a> Header<'a> {
    pub fn new(
        diag: &impl Diagnostic,
        src_file: &'a str,
        src_context: String,
        ln_col: LineColumn,
    ) -> Self {
        Self {
            severity_display: diag.severity_display(),
            msg: diag.msg(),
            src_file,
            src_context,
            ln_col,
        }
    }
}

impl<'a> fmt::Display for Header<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{}: {}\n  {} {}:{}:{} {}",
            self.severity_display,
            self.msg.bold(),
            R_ARROW.color(QUOTE),
            self.src_file,
            self.ln_col.line1(),
            self.ln_col.column1(),
            self.src_context.color(QUOTE).bold(),
        )
    }
}

/// Span for primary and secondary messages
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MsgSpan {
    /// Span in the source text
    pub span: Span,
    /// Message for the spanned text
    pub msg: Option<String>,
}

impl MsgSpan {
    pub fn new(span: Span, msg: String) -> Self {
        Self {
            span,
            msg: Some(msg),
        }
    }

    pub fn span_only(span: Span) -> Self {
        Self { span, msg: None }
    }
}

/// Window with primary message only
///
/// ```text
///      |
/// <ln> | <line_text>
///      |    ^^^^ <msg>
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PrimaryLineRender<'a> {
    pub severity: Severity,
    /// Line number for merge and display
    pub line1: u32,
    pub src_text: &'a str,
    pub line_span: Span,
    pub primary_msg: MsgSpan,
}

impl<'a> fmt::Display for PrimaryLineRender<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let n_digits = self::n_digits(self.line1 as usize);
        let indent = " ".repeat(n_digits);

        let vbar = VBAR.color(QUOTE).bold();
        let line1 = format!("{}", self.line1).color(QUOTE).bold();
        let line_text = self.line_span.slice(self.src_text).trim_end();

        writeln!(f, "{indent} {vbar}")?;
        writeln!(f, "{line1} {vbar} {line_text}")?;

        if let Some(msg) = &self.primary_msg.msg {
            writeln!(
                f,
                "{indent} {vbar} {} {}",
                self::msg_range_string(self.primary_msg.span - self.line_span.start)
                    .color(self.severity.color())
                    .bold(),
                msg.color(self.severity.color()).bold(),
            )?;
        } else {
            writeln!(
                f,
                "{indent} {vbar} {}",
                self::msg_range_string(self.primary_msg.span - self.line_span.start)
                    .color(self.severity.color())
                    .bold(),
            )?;
        }

        Ok(())
    }
}

/// Window with primary span and secondary messages
///
/// ```text
///      |
/// <ln> | <line_text>
///      |  ^ -- ---- expected `f32`, found `i32`
///      |    |
///      |    expected `u32`, found `i32`
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SecondaryLineRender<'a> {
    pub severity: Severity,
    /// Line number for merge and display
    pub line1: u32,
    pub src_text: &'a str,
    pub line_span: Span,
    pub primary_span: Option<Span>,
    pub secondary_msgs: Vec<MsgSpan>,
}

impl<'a> fmt::Display for SecondaryLineRender<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // indent for line number
        let indent = {
            let n_digits = self::n_digits(self.line1 as usize);
            " ".repeat(n_digits)
        };

        let vbar = VBAR.color(QUOTE).bold();
        let vdot = VDOT.color(QUOTE).bold();

        // <line> | <line_text>
        let line1 = format!("{}", self.line1).color(QUOTE).bold();
        let line_text = self.line_span.slice(self.src_text).trim_end();

        writeln!(f, "{line1} {vbar} {}", line_text)?;

        // markers + last sub message:
        let line_offset = self.line_span.start;
        let last_relative_span = if let Some(primary_span) = self.primary_span {
            //  |  ^
            let last_relative_span = primary_span - line_offset;

            let ws = " ".repeat(last_relative_span.start.into_usize());
            let carets = "^"
                .repeat(last_relative_span.len() as usize)
                .color(self.severity.color());

            if !self.secondary_msgs.is_empty() {
                write!(f, "{indent} {vbar} {ws}{}", carets)?;
            } else {
                // primary message only:
                writeln!(f, "{indent} {vbar} {ws}{}", carets)?;
                return Ok(());
            }

            last_relative_span
        } else {
            write!(f, "{indent} {vdot} ")?;

            assert!(!self.secondary_msgs.is_empty());

            // no relative span consumed
            Span {
                start: Offset::default(),
                end: Offset::default(),
            }
        };

        {
            //    ├──  │  ╰───
            self::print_hbar_markers(f, line_offset, last_relative_span, &self.secondary_msgs)?;

            // <msg>
            if let Some(msg) = &self.secondary_msgs.last().unwrap().msg {
                writeln!(f, " {}", msg.color(QUOTE).bold())?;
            } else {
                writeln!(f, "")?;
            }
        }

        let (vbars_string, vbars_spans) =
            self::format_vbar_pointers(line_offset, &self.secondary_msgs)?;

        if self.secondary_msgs.len() == 1 {
            return Ok(());
        }

        // show secondary messages in preceding lines:
        //       1.0  14
        // <1>:  |    |
        writeln!(f, "{indent} {vdot} {vbars_string}")?;

        // <2>:  │    ╰── <msg>
        // <2>:  ╰─ <msg>
        for i in (0..vbars_spans.len()).rev() {
            let vbar_span = &vbars_spans[i];

            let target_msg_span = &self.secondary_msgs[vbar_span.i_msg_span];
            let target_msg = target_msg_span.msg.as_ref().unwrap();

            let vbars_str = &vbars_string[0..vbars_spans[i].end_ws];

            writeln!(
                f,
                "{indent} {vdot} {vbars_str}{} {}",
                UNDER_MSG_PREFIX.color(QUOTE).bold(),
                target_msg.color(QUOTE).bold(),
            )?;
        }

        Ok(())
    }
}

///     ┌──  ╷  ─────
fn print_hbar_markers(
    f: &mut fmt::Formatter<'_>,
    line_offset: Offset,
    mut last_relative_span: Span,
    msg_spans: &[MsgSpan],
) -> fmt::Result {
    let mut spans = msg_spans.iter().peekable();

    let rect_lu = RECT_LU.color(QUOTE).bold();
    let rect_ru = RECT_RU.color(QUOTE).bold();

    while let Some(msg_span) = spans.next().map(|s| s.clone()) {
        let relative_span = msg_span.span - line_offset;
        let len = relative_span.len() as usize;

        let ws = " ".repeat(relative_span.start.into_usize() - last_relative_span.end.into_usize());

        if msg_span.msg.is_none() {
            // no need of vertical bar
            write!(f, "{ws}{}", HBAR.repeat(len).color(QUOTE).bold())?;
        } else if spans.peek().is_some() {
            match len {
                1 => {
                    write!(f, "{ws}{}", VBAR.color(QUOTE).bold())?;
                }
                2 => {
                    write!(f, "{ws}{rect_lu}{rect_ru}",)?;
                }
                _ => {
                    write!(
                        f,
                        "{ws}{rect_lu}{}{rect_ru}",
                        HBAR.repeat(len - 2).color(QUOTE).bold(),
                    )?;
                }
            }
        } else {
            // last span does not contain `┌`
            let x = RECT_LD.color(QUOTE).bold();
            let t = INV_T.color(QUOTE).bold();

            match len {
                1 => {
                    write!(f, "{ws}{x}{}", HBAR.repeat(2).color(QUOTE).bold())?;
                }
                2 => {
                    write!(f, "{ws}{x}{t}{}", HBAR.repeat(2).color(QUOTE).bold())?;
                }
                _ => {
                    write!(
                        f,
                        "{ws}{x}{}{t}{}",
                        HBAR.repeat(len - 2).color(QUOTE).bold(),
                        HBAR.repeat(2).color(QUOTE).bold(),
                    )?;
                }
            }
        }

        last_relative_span = relative_span;
    }

    Ok(())
}

/// Span returned by [`format_bars`]
#[derive(Debug)]
struct VBarSpan {
    /// Index in the original `[MsgSapn]`
    i_msg_span: usize,
    /// Span for the vbars stirngexcluding `|`
    end_ws: usize,
    /// Span for the vbars stirngincluding `|`
    end_vbar: usize,
}

/// Formats colored vertical bars `  |  |` with slices
///
/// # Example
///
/// ```text
///   │ 1.0  14 "str"
///   │ ┌──  ╷  ───── expected `f32`, found `String`
///   │ │    │
///   │ │    └─ expected `f32`, found `i32`
///   │ └─ expected because of this variable
/// ```
fn format_vbar_pointers(
    line_offset: Offset,
    msg_spans: &[MsgSpan],
) -> Result<(String, Vec<VBarSpan>), fmt::Error> {
    // NOTE: Converted to string in order to measure the length considering the color escapes
    let vbar = format!("{}", VBAR.color(QUOTE).bold());

    let mut s = String::new();
    let mut slices = Vec::new();

    // last span relative to the line offset
    let mut last_relative_span = Span {
        start: Offset::from(0u32),
        end: Offset::from(0u32),
    };

    // exclude the last message as it doesn't need vertical bar:
    let msg_spans = &msg_spans[0..msg_spans.len() - 1];

    for (i, msg_span) in msg_spans.iter().enumerate() {
        if msg_span.msg.is_none() {
            // no need of vertical bar
            continue;
        }

        let reletive_span = msg_span.span - line_offset;
        let ws_len = (reletive_span.start - last_relative_span.start) as usize;
        let ws = " ".repeat(ws_len);
        write!(s, "{ws}{vbar}")?;

        slices.push(VBarSpan {
            i_msg_span: i,
            end_ws: s.len() - vbar.len(),
            end_vbar: s.len(),
        });

        last_relative_span = reletive_span;
    }

    Ok((s, slices))
}

#[derive(Debug)]
pub struct SecondaryWindow<'a> {
    lines: Vec<SecondaryLineRender<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LineSpan {
    pub line1: u32,
    pub msg_span: MsgSpan,
    // marker: &'static str,
    // show_msg: bool,
}

impl<'a> fmt::Display for SecondaryWindow<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ln in &self.lines {
            ln.fmt(f)?;
        }

        Ok(())
    }
}

/// Diagnostic window
#[derive(Debug)]
pub enum Window<'a> {
    /// Primary message only
    Primary(PrimaryLineRender<'a>),
    /// Primary span and secondary messages
    Secondary(SecondaryWindow<'a>),
}

impl<'a> fmt::Display for Window<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primary(x) => x.fmt(f),
            Self::Secondary(x) => x.fmt(f),
        }
    }
}

/// Diagnostic rendering
#[derive(Debug)]
pub struct Render<'a> {
    /// Header (error, warning, note, hint, ..)
    pub header: Header<'a>,
    /// Main diagnostic window, typically error or warning. A sub diagnostic can have a main
    /// diagnostic window, too.
    pub window: Window<'a>,
    /// Sub diagnostic renedrings (note, hint, ..)
    pub sub_renders: Vec<Render<'a>>,
}

impl<'a> fmt::Display for Render<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // NOTE: each format contains the last newline character, so don't use `writeln!`
        write!(f, "{}", self.header)?;
        write!(f, "{}", self.window)?;
        for sub_win in &self.sub_renders {
            writeln!(f, "")?;
            write!(f, "{}", sub_win)?;
        }
        Ok(())
    }
}

pub fn window_multi_msg<'a>(
    db: &'a dyn BaseDb,
    severity: Severity,
    input_file: InputFile,
    primary_span: Span,
    secondary_msgs: Vec<MsgSpan>,
) -> Window<'a> {
    let src_text = input_file.source_text(db);

    let (line_span, ln_col) = {
        let ln_tbl = input_file.line_column_table(db);
        (
            ln_tbl.line_span(primary_span.start),
            ln_tbl.line_column(primary_span.start),
        )
    };

    let ln_tbl = input_file.line_column_table(db);

    let primary_span_line1 = ln_tbl.line_column(primary_span.start).line1();

    // FIXME: efficiency
    let (line_spans, contains_primary_span) = {
        let mut line_spans = secondary_msgs
            .into_iter()
            .map(|msg_span| LineSpan {
                line1: ln_tbl.line_column(msg_span.span.start).line1(),
                msg_span,
            })
            .collect::<Vec<_>>();

        // sort by `line1` then `msg_span.start`
        // TODO: repolace with `Ord` impl
        use std::cmp::Ordering;
        line_spans.sort_unstable_by(|x1, x2| match x1.line1.cmp(&x2.line1) {
            Ordering::Equal => x1.msg_span.span.start.cmp(&x2.msg_span.span.start),
            x => x,
        });

        let contains_primary_span = line_spans.iter().any(|sp| sp.line1 == primary_span_line1);

        (line_spans, contains_primary_span)
    };

    let mut line_renders = Vec::new();

    // primary span only
    if !contains_primary_span {
        let line_render = SecondaryLineRender {
            severity,
            line1: ln_col.line1(),
            src_text,
            line_span,
            primary_span: Some(primary_span),
            secondary_msgs: vec![],
        };

        line_renders.push(line_render);
    }

    // primary span + secondary spans
    use itertools::Itertools;
    for (line1, group) in &line_spans.into_iter().group_by(|line_span| line_span.line1) {
        let primary_span = if line1 == primary_span_line1 {
            Some(primary_span)
        } else {
            None
        };

        let secondary_msgs = group.map(|group| group.msg_span).collect::<Vec<_>>();
        let line_span = ln_tbl.line_span(secondary_msgs[0].span.start);

        let line_render = SecondaryLineRender {
            severity,
            line1,
            src_text,
            line_span,
            primary_span,
            secondary_msgs,
        };

        line_renders.push(line_render);
    }

    Window::Secondary(SecondaryWindow {
        lines: line_renders,
    })
}

pub fn get_header<'a>(
    db: &'a dyn BaseDb,
    diag: &impl Diagnostic,
    input_file: InputFile,
    src_context: String,
    primary_span: Span,
) -> (Header<'a>, Span, &'a str) {
    let src_file = input_file.name(db).as_str(db);
    let ln_tbl = input_file.line_column_table(db);
    let ln_col = ln_tbl.line_column(primary_span.start);
    let header = Header::new(diag, src_file, src_context, ln_col);

    let line_span = ln_tbl.line_span(primary_span.start);
    let src_text = input_file.source_text(db);

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
