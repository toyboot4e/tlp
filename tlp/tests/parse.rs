/*!
Run all test cases in `parse/cases.txt` (on `cargo test`)
*/

use std::fmt::{self, Write};

use tlp::syntax::cst::{self, data::SyntaxElement, parse::ParseError};

// TODO: use slice
#[derive(Debug, Clone)]
struct Test {
    title: String,
    code: String,
    expected: String,
}

#[derive(Debug, Clone)]
struct TestError {
    test: Test,
    ast: String,
}

impl fmt::Display for TestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}
--- code
{}
--- ast
{}
--- expected
{}",
            self.test.title, self.test.code, self.ast, self.test.expected,
        )
    }
}

fn run_test(test: Test) -> Result<(), TestError> {
    let (tree, errs) = cst::parse::from_str(&test.code);

    if !errs.is_empty() {
        let s = errs
            .iter()
            .map(|e| format!("{}", e.with_loc(&test.code)))
            .collect::<Vec<_>>()
            .join(", ");
        panic!("{}", s);
    }

    // root
    assert_eq!(
        format!("{:?}", tree),
        format!("ROOT@0..{}", test.code.len())
    );

    let mut nest = 0;
    let ast_repr = tree
        .children_with_tokens()
        .flat_map(|elem| match elem {
            SyntaxElement::Node(node) => node
                .preorder_with_tokens()
                .filter_map(|ev| match ev {
                    rowan::WalkEvent::Enter(node) => {
                        let last_nest = nest;
                        nest += 1;
                        Some((last_nest, node))
                    }
                    rowan::WalkEvent::Leave(_) => {
                        nest -= 1;
                        None
                    }
                })
                .collect::<Vec<_>>(),
            SyntaxElement::Token(_) => vec![(nest, elem)],
        })
        .map(|(nest, child)| {
            format!(
                "{}{:?}@{:?}",
                "    ".repeat(nest),
                child.kind(),
                child.text_range()
            )
        })
        .collect::<Vec<_>>();

    let ast = ast_repr.join("\n");
    let expected = test.expected.trim();

    if ast == expected {
        Ok(())
    } else {
        Err(TestError { test, ast })
    }
}

fn is_ws(ln: &str) -> bool {
    ln.bytes()
        .skip_while(|b| matches!(b, b' ' | b'\t'))
        .next()
        .is_none()
}

#[test]
fn parse() {
    let src = include_str!("parse/cases.txt");

    let mut chunks = {
        // 40 - s
        let delim = "----------------------------------------";
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
            .skip_while(|ln| ln.starts_with("//") || is_ws(ln));

        let title = header.next().unwrap();
        let code = header.collect::<Vec<&str>>().join("\n");

        tests.push(Test {
            title: title.trim().to_string(),
            code: code.trim().to_string(),
            expected: expected.to_string(),
        });
    }

    let mut errs = vec![];
    for test in tests {
        if let Err(err) = self::run_test(test) {
            errs.push(err);
        }
    }

    if errs.is_empty() {
        return;
    }

    for e in &errs {
        eprintln!("{}", e);
    }
    panic!();
}
