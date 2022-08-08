//! Creates `hir_def` types from AST

mod body;
mod item;
mod modules;

pub use self::{body::*, item::*, modules::*};
