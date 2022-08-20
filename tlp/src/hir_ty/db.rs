//! Incremental computation powered by [`salsa`]

use std::fmt;

use crate::hir_def::db::*;

/// [`salsa`] database instance for the queries
#[salsa::database(SourceDB, ParseDB, InternDB, DefDB, HirDB)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl fmt::Debug for DB {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("DB").finish()
    }
}

impl salsa::Database for DB {}

impl Upcast<dyn Intern> for DB {
    fn upcast(&self) -> &(dyn Intern + 'static) {
        &*self
    }
}

#[salsa::query_group(HirDB)]
pub trait Hir: Def + Parse + Intern + Upcast<dyn Intern> {
    //
}
