/*!
Abstract syntax tree, typed tree layered on top of CST

All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just wrappers
around CST nodes. Each component is lazily retrieved via accessors traversing the internal CST.
*/

pub mod data;
pub mod validate;

use crate::syntax::cst::parse::{self, ParseError};

use data::Document;

pub fn parse(src: &str) -> (Document, Vec<ParseError>) {
    let (cst, errs) = parse::from_str(src);
    (Document::from_root(cst).unwrap(), errs)
}

// #[cfg(test)]
// mod test {
//     use crate::syntax::ast;
//
//     #[test]
//     fn proc() {
//         let src = "(proc (x y z) (run-some-code))";
//
//         let (ast, errs) = ast::parse(src);
//
//         if !errs.is_empty() {
//             let mut s = String::new();
//             for e in errs {
//                 s.push_str(&format!("{:?}", e));
//             }
//             panic!(s);
//         }
//
//         let defun = ast.items().next().unwrap().as_proc().unwrap();
//         assert_eq!(defun.params().unwrap().arity(), 3);
//     }
// }
