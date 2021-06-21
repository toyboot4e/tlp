/*!
Concrete syntax tree, oufput of first pass

The CST is powered by [`rowan`]. It manages lossless structure of text, represented as a tree of
untyped, homogeneous tokens.
*/

pub mod data;
pub mod lex;
pub mod parse;

#[doc(inline)]
pub use parse::from_str as parse;
