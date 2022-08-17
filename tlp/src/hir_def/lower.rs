//! Creates [`hir_def`](crate::hir_def) types from AST

mod lower_body;
mod lower_item;
mod lower_item_scope;

pub use self::{lower_body::*, lower_item::*, lower_item_scope::*};
