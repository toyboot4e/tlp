//! Intermediate representation
//!
//! The primary API is built on [`InputFileExt`] and [`item::Proc`].
//!
//! Passes: [`item`] → [`body`] → [`ty`]

pub mod body;
pub mod ir_diag;
pub mod item;
pub mod item_scope;
pub mod jar;
pub mod resolve;
pub mod ty;

mod lower_ir;

use salsa::DebugWithDb;

/// List of salsa contents associated with [`IrDb`]
#[salsa::jar(db = IrDb)]
pub struct IrJar(
    jar::ParsedFile,
    item::Proc,
    // TODO: Consider ditching? Though it adds lifetimes to `Resolver`
    item_scope::ItemScope,
    body::Body,
    ty::Ty,
    ty::ty_diag::TypeDiagnostics,
    // TODO: Consider ditching? Thoguh it adds lifetimes to `Resolver`
    body::expr_scope::ExprScopeMap,
    ir_diag::ItemDiagnostics,
    lower_ir::lower_items,
    lower_ir::lower_item_scope,
    lower_ir::lower_body,
    lower_ir::lower_proc_expr_scope,
    ty::lower_type::lower_body_types,
    ty::lower_type::lower_proc_type,
);

/// salsa database for [`IrJar`] contents
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
    fn parse_items(self, db: &dyn IrDb) -> &jar::ParsedFile;
    fn items(self, db: &dyn IrDb) -> &[item::Item];
    fn item_scope<'db>(self, db: &'db dyn IrDb) -> item_scope::ItemScope;
    fn resolver(self, db: &dyn IrDb) -> resolve::Resolver;
    fn item_syntax_diags(self, db: &dyn IrDb) -> Vec<ir_diag::ItemDiagnostic>;
}

/// Extensions
impl InputFileExt for base::jar::InputFile {
    fn parse_items(self, db: &dyn IrDb) -> &jar::ParsedFile {
        lower_ir::lower_items(db, self)
    }

    fn items(self, db: &dyn IrDb) -> &[item::Item] {
        lower_ir::lower_items(db, self).items(db)
    }

    fn item_scope<'db>(self, db: &'db dyn IrDb) -> item_scope::ItemScope {
        lower_ir::lower_item_scope(db, self)
    }

    fn resolver(self, db: &dyn IrDb) -> resolve::Resolver {
        resolve::resolver_for_file(db, self)
    }

    fn item_syntax_diags(self, db: &dyn IrDb) -> Vec<ir_diag::ItemDiagnostic> {
        lower_ir::lower_items::accumulated::<ir_diag::ItemDiagnostics>(db, self)
    }
}

/// # Incremental computation
impl item::Proc {
    pub fn body(&self, db: &dyn IrDb) -> body::Body {
        lower_ir::lower_body(db, *self)
    }

    /// Just a shorthand
    pub fn body_data<'db>(&self, db: &'db dyn IrDb) -> &'db body::BodyData {
        let body = lower_ir::lower_body(db, *self);
        body.data(db)
    }

    /// Just a shorthand
    pub fn body_spans<'db>(&self, db: &'db dyn IrDb) -> &'db body::BodySpans {
        let body = lower_ir::lower_body(db, *self);
        body.spans(db)
    }

    /// Maybe use resolvers
    pub fn expr_scopes<'db>(&self, db: &'db dyn IrDb) -> body::expr_scope::ExprScopeMap {
        lower_ir::lower_proc_expr_scope(db, *self)
    }

    /// Body type table
    pub fn type_table<'db>(&self, db: &'db dyn IrDb) -> &'db ty::TypeTable {
        ty::lower_type::lower_body_types(db, *self)
    }

    /// Returns type diagnostics accumulated on [`Self::type_table`]
    pub fn body_ty_diags<'db>(&self, db: &'db dyn IrDb) -> Vec<ty::ty_diag::TypeDiagnostic> {
        ty::lower_type::lower_body_types::accumulated::<ty::ty_diag::TypeDiagnostics>(db, *self)
    }

    /// Resolver for the procedure type (parameters and return type)
    pub fn proc_ty_resolver(&self, db: &dyn IrDb) -> resolve::Resolver {
        self.input_file(db).resolver(db)
    }

    pub fn ty(&self, db: &dyn IrDb) -> ty::Ty {
        ty::lower_type::lower_proc_type(db, *self)
    }

    /// Returns type diagnostics accumulated on [`Self::ty`]
    pub fn param_ty_diags<'db>(&self, db: &'db dyn IrDb) -> Vec<ty::ty_diag::TypeDiagnostic> {
        ty::lower_type::lower_proc_type::accumulated::<ty::ty_diag::TypeDiagnostics>(db, *self)
    }

    /// Just a shorthand
    pub fn ty_data<'db>(&self, db: &'db dyn IrDb) -> &'db ty::TypeData {
        self.ty(db).data(db)
    }

    /// Just a shorthand
    pub fn ty_data_as_proc<'db>(&self, db: &'db dyn IrDb) -> &'db ty::ProcType {
        match self.ty_data(db) {
            ty::TypeData::Proc(proc) => proc,
            _ => unreachable!(
                "bug: procedure lowered into non-procudure type: {:?}",
                self.debug(db)
            ),
        }
    }
}

/// # Extensions
/// Ideally resolvers should be both more efficient and stateless.
impl item::Proc {
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
}
