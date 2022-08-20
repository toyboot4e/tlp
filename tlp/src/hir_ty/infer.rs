//! Walks through procedure body and assigns types to each exrpression and pattern

#![allow(unused)]

use std::ops;

use la_arena::{ArenaMap, Idx};

use crate::{
    hir_def::{
        body::{expr::Expr, pat::Pat, Body},
        ids::*,
        item_list::item,
    },
    hir_ty::{db::*, ty::Ty},
};

/// Mapping of expressions and patterns to types
#[derive(Debug, Clone)]
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

/// Infers
pub(crate) fn infer_proc(db: &dyn Hir, proc: HirItemLocId<item::DefProc>) -> InferenceResult {
    todo!()
}

/// The inference context contains all information needed during type inference.
#[derive(Clone)]
pub(crate) struct Infer<'a> {
    pub(crate) db: &'a dyn Hir,
    // pub(crate) owner: DefWithBodyId,
    pub(crate) body: &'a Body,
    // pub(crate) resolver: Resolver,
    pub(crate) result: InferenceResult,
    return_ty: Ty,
}
