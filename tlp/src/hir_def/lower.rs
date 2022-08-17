//! Creates [`hir_def`](crate::hir_def) types from AST

mod body;
mod item;
mod item_scope;

pub use self::{body::*, item::*, item_scope::*};
