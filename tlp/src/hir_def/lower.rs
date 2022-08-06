//! Converts ASTs into an interned, ID-based representation

mod items;
mod modules;
mod scope;

pub use self::{items::*, modules::*, scope::*};
