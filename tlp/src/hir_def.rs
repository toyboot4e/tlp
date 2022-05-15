//! Intermadiate representations between AST and bytecode
//!
//! # Data flow
//!
//! * macro expansion: ?
//! * name resolution: AST → ItemTree → ?
//! * type inference: ?
//!
//! # TODO
//!
//! * Source map pattern (AstID → AST), and the opposite?

pub mod data;
pub mod db;
mod lower;
