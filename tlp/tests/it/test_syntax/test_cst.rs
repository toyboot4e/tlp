//! Run all test cases in `cst_test_cases.txt` (on `cargo test`)

use std::fmt::Write;

use tlp::syntax::cst::{self, SyntaxElement, SyntaxNode};

use crate::test_syntax::{self, Test, TestError};

fn cst_display(cst: &SyntaxNode) -> String {
    let mut nest = 0;

    let repr = cst
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

    repr.join("\n")
}

fn run_test(test: Test) -> Result<(), TestError> {
    let (tks, errs) = cst::lex::from_str(&test.code);
    assert!(errs.is_empty(), "{:?}", errs);

    let (cst, errs) = cst::parse(&test.code, &tks);

    if !errs.is_empty() {
        let s = errs
            .iter()
            // FIXME: print with location
            .map(|e| format!("- {}", e))
            .collect::<Vec<_>>()
            .join("\n");
        panic!("Parse error:\n{}\nsource: {}", s, test.code);
    }

    // root
    assert_eq!(format!("{:?}", cst), format!("ROOT@0..{}", test.code.len()));

    let cst_string = self::cst_display(&cst);
    let expected = test.expected.trim();

    if cst_string == expected {
        Ok(())
    } else {
        Err(TestError {
            test,
            output: cst_string,
        })
    }
}

#[test]
fn cst() {
    let src = include_str!("cst_test_cases.txt");
    let tests = test_syntax::collect_tests(src);

    let errs = tests
        .into_iter()
        .filter_map(|t| self::run_test(t).err())
        .collect::<Vec<_>>();

    if errs.is_empty() {
        return;
    }

    let mut s = String::new();
    writeln!(s, "Errors:").unwrap();
    for e in &errs {
        writeln!(s, "- {}", e).unwrap();
        writeln!(s, "").unwrap();
    }

    panic!("{}", s);
}
