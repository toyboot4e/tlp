//! Converts ASTs into an interned, ID-based representation

mod item;
mod modules;
mod scope;

pub use self::{item::*, modules::*, scope::*};
