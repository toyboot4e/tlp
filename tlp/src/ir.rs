//! Intermediate representation

pub mod body;
pub mod item;
pub mod item_scope;
pub mod jar;
pub mod lower;
pub mod resolve;
pub mod ty;

#[salsa::jar(db = IrDb)]
pub struct IrJar(
    jar::ParsedFile,
    item::Proc,
    // TODO: Consider ditching? Though it adds lifetimes to `Resolver`
    item_scope::ItemScope,
    body::Body,
    // TODO: Consider ditching? Thoguh it adds lifetimes to `Resolver`
    body::expr_scope::ExprScopeMap,
    lower::lower_items,
    lower::lower_item_scope,
    lower::lower_body,
    lower::lower_proc_expr_scope,
    ty::lower_type::lower_body_types,
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
        lower::lower_proc_expr_scope(db, *self)
    }

    /// Resolver for the root block expression
    pub fn root_resolver<'db>(&self, db: &'db dyn IrDb) -> resolve::Resolver {
        let root_expr = self.body_data(db).root_block;
        resolve::resolver_for_proc_expr(db, *self, root_expr)
    }

    /// Resolver for a expression in this procedure
    pub fn expr_resolver<'db>(
        &self,
        db: &'db dyn IrDb,
        expr: body::expr::Expr,
    ) -> resolve::Resolver {
        resolve::resolver_for_proc_expr(db, *self, expr)
    }

    pub fn type_table<'db>(&self, db: &'db dyn IrDb) -> &'db ty::TypeTable {
        ty::lower_type::lower_body_types(db, *self)
    }
}
