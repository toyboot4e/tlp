/*!
Concrete syntax tree

`cst` is powered by [`rowan`]. It manages lossless structure of text, represented as a tree (node)
of untyped, homogeneous tokens.

Semanctic information is latter added by casting the CST nodes to AST nodes.
*/

pub mod data;
pub mod lex;
pub mod parse;

#[doc(inline)]
pub use parse::from_str as parse;
