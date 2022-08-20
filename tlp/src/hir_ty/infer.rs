//! Walks through procedure body and assigns types to each exrpression and pattern

use std::{ops, sync::Arc};

use la_arena::{ArenaMap, Idx};

use crate::{
    hir_def::{
        body::{expr::Expr, pat::Pat, Body},
        ids::*,
        item_list::item,
    },
    hir_ty::{db::*, ty::Ty},
};

/// Infers
pub(crate) fn infer_proc_query(
    db: &dyn Hir,
    proc: HirItemLocId<item::DefProc>,
) -> Arc<InferenceResult> {
    let body = db.proc_body(proc);
    let unknown_ty = 0; // FIXME

    let mut infer = Infer {
        result: InferenceResult::new(unknown_ty),
        db,
        body: &body,
    };

    infer.infer_proc_body();

    Arc::new(infer.result)
}

/// Mapping of expressions and patterns to types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceResult {
    expr_types: ArenaMap<Idx<Expr>, Ty>,
    /// For each pattern record the type it resolves to.
    ///
    /// **Note**: When a pattern type is resolved it may still contain
    /// unresolved or missing subpatterns or subpatterns of mismatched types.
    pat_types: ArenaMap<Idx<Pat>, Ty>,
    unknown_ty: Ty,
}

impl ops::Index<Idx<Expr>> for InferenceResult {
    type Output = Ty;

    fn index(&self, expr: Idx<Expr>) -> &Ty {
        self.expr_types.get(expr).unwrap_or(&self.unknown_ty)
    }
}

impl ops::Index<Idx<Pat>> for InferenceResult {
    type Output = Ty;

    fn index(&self, pat: Idx<Pat>) -> &Ty {
        self.pat_types.get(pat).unwrap_or(&self.unknown_ty)
    }
}

impl InferenceResult {
    fn new(unknown_ty: Ty) -> Self {
        Self {
            expr_types: Default::default(),
            pat_types: Default::default(),
            unknown_ty,
        }
    }
}

/// The inference context contains all information needed during type inference.
#[derive(Clone)]
pub(crate) struct Infer<'a> {
    /// Output
    pub(crate) result: InferenceResult,
    pub(crate) db: &'a dyn Hir,
    // pub(crate) owner: DefWithBodyId,
    pub(crate) body: &'a Body,
    // pub(crate) resolver: Resolver,
    // return_ty: Ty,
}

/// Write result
impl<'a> Infer<'a> {
    fn write_expr_ty(&mut self, expr: Idx<Expr>, ty: Ty) {
        self.result.expr_types.insert(expr, ty);
    }

    fn write_pat_ty(&mut self, pat: Idx<Pat>, ty: Ty) {
        self.result.pat_types.insert(pat, ty);
    }
}

impl<'a> Infer<'a> {
    pub fn infer_proc_body(&mut self) {
        let root = self.body.root_block;
        self.infer_expr(root);
    }
}

/// Infer expression types
impl<'a> Infer<'a> {
    pub fn infer_expr(&mut self, expr_idx: Idx<Expr>) {
        let expr = &self.body.exprs[expr_idx];
        match expr {
            Expr::Block(block) => {
                todo!()
            }
            Expr::Literal(lit) => todo!("{:?}", lit),
            Expr::Let(let_) => todo!("{:?}", let_),
            _ => todo!("{:?}", expr),
        }
    }
}
