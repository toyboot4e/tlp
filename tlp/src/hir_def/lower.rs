//! Creates [`hir_def`](crate::hir_def) types from AST
//!
//! Lowering is converting AST data into `Arena` s.

mod lower_body;
mod lower_item;
mod lower_item_scope;

pub use self::{lower_body::*, lower_item::*, lower_item_scope::*};
