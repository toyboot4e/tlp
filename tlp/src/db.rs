/*!
`salsa` integation where incremental computation is performed
*/

pub mod intern;
pub mod queries;

use self::{intern::InternDB, queries::*};

/// `salsa` database for the [`queries`]
#[salsa::database(SourceDB, ParseDB, InternDB, LowerModuleDB)]
#[derive(Default)]
pub struct DB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DB {}
