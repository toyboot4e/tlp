/*!
Run all test cases in `cst/cases.txt` (on `cargo test`)
*/

use std::fmt::{self, Write};

use tlp::syntax::cst::{
    self,
    data::{SyntaxElement, SyntaxNode},
    parse::ParseError,
};

use crate::utils::{self, Test, TestError};

fn display(cst: &SyntaxNode) -> String {
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
    let (cst, errs) = cst::parse::from_str(&test.code);

    if !errs.is_empty() {
        let s = errs
            .iter()
            .map(|e| format!("{}", e.with_loc(&test.code)))
            .collect::<Vec<_>>()
            .join(", ");
        panic!("{}", s);
    }

    // root
    assert_eq!(format!("{:?}", cst), format!("ROOT@0..{}", test.code.len()));

    let cst_string = self::display(&cst);
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
    let src = include_str!("cst/cases.txt");
    let tests = utils::collect_tests(src);

    let errs = tests
        .into_iter()
        .filter_map(|t| self::run_test(t).err())
        .collect::<Vec<_>>();

    if errs.is_empty() {
        return;
    }

    for e in &errs {
        eprintln!("{}", e);
        eprintln!("");
    }

    panic!();
}
