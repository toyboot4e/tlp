//! Procecures for creating `hir_def` data types
//!
//! Lowering is about converting AST data into `Arena` s.

mod collect_item_scope;
mod lower_body;
mod lower_item;

pub use self::{collect_item_scope::*, lower_body::*, lower_item::*};
