//! Tests for `syntax` module.

use tlp::syntax::cst::{self, SyntaxElement, SyntaxNode};

use crate::util::{self, Test, TestError};

/// Runs tests defined in the test case files
#[test]
fn cst() {
    let src = include_str!("test_cases/cst_test_cases.txt");
    util::run_tests(src, runner)
}

fn runner(test: Test) -> Result<(), TestError> {
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
    test.result(&cst_string)
}

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
