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
//! * Source map pattern (AST ID → AST)
//! * Source map pattern (AST → AST ID)

pub mod item;
pub mod def;
pub mod decl;
pub mod db;
pub mod res;

mod lower;
