//! Converts ASTs into an interned, ID-based representation

mod decls;
mod modules;
mod scope;

pub use self::{decls::*, modules::*, scope::*};
