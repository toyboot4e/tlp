/*!
Concrete syntax tree powered by [`rowan`]

[`rowan`] manages lossless structure of text, represented as a tree of untyped, homogeneous tokens.
*/

pub mod data;
pub mod lex;
pub mod parse;

#[doc(inline)]
pub use parse::from_str as parse;
