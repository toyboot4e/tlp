//! Test utilities, including data-driven ones.

use std::fmt::{self, Write};

use rowan::TextSize;

pub const CURSOR_MARKER: &str = "$0";
pub const ESCAPED_CURSOR_MARKER: &str = "\\$0";

/// Returns the offset of the first occurrence of `$0` marker and the copy of `text`
/// without the marker.
pub fn try_extract_offset(text: &str) -> Option<(TextSize, String)> {
    let cursor_pos = text.find(CURSOR_MARKER)?;
    let mut new_text = String::with_capacity(text.len() - CURSOR_MARKER.len());
    new_text.push_str(&text[..cursor_pos]);
    new_text.push_str(&text[cursor_pos + CURSOR_MARKER.len()..]);
    let cursor_pos = TextSize::from(cursor_pos as u32);
    Some((cursor_pos, new_text))
}

/// Asserts that two strings are equal, otherwise displays a rich diff between them.
///
/// The diff shows changes from the "original" left string to the "actual" right string.
///
/// All arguments starting from and including the 3rd one are passed to
/// `eprintln!()` macro in case of text inequality.
#[macro_export]
macro_rules! assert_eq_text {
    ($left:expr, $right:expr) => {
        $crate::util::assert_eq_text!($left, $right,)
    };
    ($left:expr, $right:expr, $($tt:tt)*) => {{
        let left = $left;
        let right = $right;
        if left != right {
            if left.trim() == right.trim() {
                std::eprintln!("Left:\n{:?}\n\nRight:\n{:?}\n\nWhitespace difference\n", left, right);
            } else {
                std::eprintln!("Left:\n{:?}\n\nRight:\n{:?}\n\nWhitespace difference\n", left, right);
                // FIXME:
                // let diff = $crate::__diff(left, right);
                // std::eprintln!("Left:\n{}\n\nRight:\n{}\n\nDiff:\n{}\n", left, right, $crate::format_diff(diff));
            }
            std::eprintln!($($tt)*);
            panic!("text differs");
        }
    }};
}

pub use assert_eq_text;

pub type TestResult<T = ()> = Result<T, TestError>;

/// Parsed test case
#[derive(Debug, Clone)]
pub struct Test {
    pub title: String,
    pub code: String,
    pub expected: String,
}

impl Test {
    pub fn result(self, output: &str) -> TestResult {
        // REMARK: trimmed
        if output.trim() == self.expected.trim() {
            Ok(())
        } else {
            Err(TestError {
                test: self.clone(),
                output: output.to_string(),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct TestError {
    pub test: Test,
    pub output: String,
}

impl fmt::Display for TestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}
--- code:
{}
--- output:
{}
--- expected:
{}",
            self.test.title, self.test.code, self.output, self.test.expected,
        )
    }
}

pub fn run_tests(src: &str, runner: fn(Test) -> TestResult) {
    let tests = self::collect_tests(src);

    let errs = tests
        .into_iter()
        .filter_map(|t| runner(t).err())
        .collect::<Vec<_>>();

    if errs.is_empty() {
        return;
    }

    let mut s = String::new();
    writeln!(s, "Errors:").unwrap();
    for e in &errs {
        writeln!(s, "{}", e).unwrap();
        writeln!(s, "").unwrap();
    }

    panic!("{}", s);
}

pub fn collect_tests(src: &str) -> Vec<Test> {
    let mut chunks = {
        // 40 hyphens
        let delim = r#"----------------------------------------"#;
        src.split(delim)
    };

    let mut tests = vec![];
    while let Some(header) = chunks.next() {
        let expected = match chunks.next() {
            Some(block) => block,
            None => break,
        };

        // TODO: use slice
        let mut header = header
            .lines()
            .filter(|ln| !ln.starts_with("//"))
            .skip_while(|ln| is_ws(ln));

        // first line: title
        let title = match header.next() {
            Some(t) => t,
            None => break,
        };

        // other lines: code
        let code = header.collect::<Vec<&str>>().join("\n");

        tests.push(Test {
            title: title.trim().to_string(),
            code: code.trim().to_string(),
            // TODO: proper trim handling
            expected: expected.to_string(),
        });
    }

    tests
}

fn is_ws(ln: &str) -> bool {
    ln.bytes()
        .skip_while(|b| matches!(b, b' ' | b'\t'))
        .next()
        .is_none()
}
