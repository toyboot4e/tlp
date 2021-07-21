/*!
Abstract syntax tree, typed tree layered on top of CST

All AST nodes have the same internal structure, i.e, CST. As a result, AST nodes are just wrappers
around CST nodes. Each component is lazily retrieved via accessors traversing the internal CST.
*/

pub mod data;
pub mod validate;

pub use crate::syntax::cst::parse::ParseError;

use crate::syntax::cst;

use data::Document;

pub fn parse(src: &str) -> (Document, Vec<ParseError>) {
    let (cst, errs) = cst::parse::from_str(src);
    let doc = Document::from_root(cst).unwrap();
    (doc, errs)
}
