/*!
Common utilities for tests
*/

use std::fmt::{self};

use tlp::syntax::cst::{self, ParseError, SyntaxElement};

#[derive(Debug, Clone)]
pub struct Test {
    pub title: String,
    pub code: String,
    pub expected: String,
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

pub fn collect_tests(src: &str) -> Vec<Test> {
    let mut chunks = {
        // 40 hyphens
        let delim = r#"\n----------------------------------------\n"#;
        src.split(delim)
    };

    let mut tests = vec![];
    while let Some(header) = chunks.next() {
        let expected = match chunks.next() {
            Some(block) => block.trim(),
            None => break,
        };

        // TODO: use slice
        let mut header = header
            .lines()
            .filter(|ln| ln.starts_with("//"))
            .skip_while(|ln| is_ws(ln));

        let title = header.next().unwrap();
        let code = header.collect::<Vec<&str>>().join("\n");

        tests.push(Test {
            title: title.trim().to_string(),
            code: code.trim().to_string(),
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
