//! Syntax tree (source code → CST → AST)
//!
//! # [`rowan`]-based syntax tree
//!
//! The source file is first parsed into a concrete syntax tree, preserving all text information
//! including whitespaces. It's IDE-friendly in that we can easily store invalid syntax and continue
//! analyzing it.
//!
//! AST nodes are view to CST nodes, constructed as casts of them. They do not have explicit
//! components, but they search for the requested component from the underlying CST on demand.
//!
//! # Dependency note
//!
//! The `syntax` module is not using `salsa`, except for the diagnostic rendering. The CST/AST are
//! created with [`rowan`] and treated as fragile and soon be abandoned after creating the IR. We
//! might rather want to have our own token tree / S-expression storage as in the `dada` langauge.

pub mod ast;
pub mod cst;
pub mod ptr;

mod syn_diag;
