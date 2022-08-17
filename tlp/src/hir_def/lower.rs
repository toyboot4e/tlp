//! Procecures for creating `hir_def` data types
//!
//! Lowering is about converting AST data into `Arena` s.

// lower
mod lower_body;
mod lower_item;

// collect
mod lower_item_scope;

pub use self::{lower_body::*, lower_item::*, lower_item_scope::*};
