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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseResult {
    pub doc: data::Document,
    pub errs: Vec<ParseError>,
}

impl ParseResult {
    pub fn into_tuple(self) -> (data::Document, Vec<ParseError>) {
        (self.doc, self.errs)
    }
}

pub fn parse(src: &str) -> ParseResult {
    let (cst, errs) = cst::parse::from_str(src);
    let doc = Document::from_root(cst).unwrap();
    ParseResult { doc, errs }
}
