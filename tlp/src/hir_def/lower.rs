//! Converts ASTs into an interned, ID-based representation

mod data;
mod items;
mod modules;

pub use self::{data::*, items::*, modules::*};
