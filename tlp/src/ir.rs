//! Intermediate representation

pub mod body;
pub mod item;
pub mod item_scope;
pub mod jar;
pub mod lower;
pub mod resolve;

#[salsa::jar(db = IrDb)]
pub struct IrJar(
    jar::ParsedFile,
    lower::lower_items,
    lower::lower_body,
    lower::lower_item_scope,
    item::Proc,
    // TODO: Consider ditching? It adds lifetimes to `Resolver` though
    item_scope::ItemScope,
    body::Body,
    body::expr_scope::proc_expr_scope_query,
    // TODO: Consider ditching? It adds lifetimes to `Resolver` though
    body::expr_scope::ExprScopeMap,
);

pub trait IrDb: salsa::DbWithJar<IrJar> + base::BaseDb {
    fn as_ir_db(&self) -> &dyn IrDb;
}

impl<T: salsa::DbWithJar<IrJar> + base::BaseDb> IrDb for T {
    fn as_ir_db(&self) -> &dyn IrDb {
        self
    }
}

/// Extensions
pub trait InputFileExt {
    fn source_file(self, db: &dyn IrDb) -> &jar::ParsedFile;
    fn items(self, db: &dyn IrDb) -> &[item::Item];
    fn item_scope<'db>(self, db: &'db dyn IrDb) -> item_scope::ItemScope;
}

/// Extensions
impl InputFileExt for base::jar::InputFile {
    fn source_file(self, db: &dyn IrDb) -> &jar::ParsedFile {
        lower::lower_items(db, self)
    }

    fn items(self, db: &dyn IrDb) -> &[item::Item] {
        lower::lower_items(db, self).items(db)
    }

    fn item_scope<'db>(self, db: &'db dyn IrDb) -> item_scope::ItemScope {
        lower::lower_item_scope(db, self)
    }
}

impl item::Proc {
    pub fn body(&self, db: &dyn IrDb) -> body::Body {
        lower::lower_body(db, *self)
    }

    pub fn body_data<'db>(&self, db: &'db dyn IrDb) -> &'db body::BodyData {
        let body = lower::lower_body(db, *self);
        body.data(db)
    }

    pub fn body_spans<'db>(&self, db: &'db dyn IrDb) -> &'db body::BodySpans {
        let body = lower::lower_body(db, *self);
        body.spans(db)
    }

    pub fn expr_scopes<'db>(&self, db: &'db dyn IrDb) -> body::expr_scope::ExprScopeMap {
        body::expr_scope::proc_expr_scope_query(db, *self)
    }
}
