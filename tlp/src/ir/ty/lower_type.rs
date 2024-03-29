//! Type lowering

// TODO: Nevernot infer twice
// TODO: Never overwrite known types on inference

use std::cmp;

use rustc_hash::FxHashMap;
use salsa::DebugWithDb;
use typed_index_collections::TiVec;

use crate::ir::{
    body::{
        expr::{self, Expr, ExprData},
        expr_debug::DebugContext,
        pat::{Pat, PatData},
        BodyData,
    },
    item,
    resolve::{Resolver, ValueNs},
    ty::{
        self,
        ty_diag::{self, *},
        Ty, TyIndex, TypeData, TypeTable, WipTypeData,
    },
    IrDb, IrJar,
};

#[salsa::tracked(jar = IrJar, return_ref)]
pub(crate) fn lower_body_types(db: &dyn IrDb, proc: item::Proc) -> TypeTable {
    let interned = InternedTypes::new(db);

    let debug = proc.with_db(db);

    let Collect {
        db,
        body_data,
        debug: _,
        interned,
        proc,
        expr_types,
        pat_types,
        types,
    } = {
        let mut collect = Collect {
            db,
            body_data: proc.body_data(db),
            debug,
            interned,
            proc,
            // resolver: proc.root_resolver(db),
            expr_types: Default::default(),
            pat_types: Default::default(),
            types: Default::default(),
        };

        collect.collect();
        collect
    };

    let mut infer = Infer {
        db,
        body_data,
        // debug,
        interned,
        proc,
        expr_types: &expr_types,
        pat_types: &pat_types,
        types,
    };
    infer.infer_all();

    // unwrap all
    let types = infer
        .types
        .into_iter()
        .map(|ty| match ty {
            WipTypeData::Var => infer.interned.unknown,
            WipTypeData::Ty(ty) => ty,
        })
        .collect();

    TypeTable {
        expr_types,
        pat_types,
        types,
    }
}

#[derive(Debug, Clone)]
struct InternedTypes {
    bool_: Ty,
    i32_: Ty,
    f32_: Ty,
    stmt: Ty,
    unknown: Ty,
}

impl InternedTypes {
    pub fn new(db: &dyn IrDb) -> Self {
        Self {
            bool_: Ty::intern(db, TypeData::Primitive(ty::PrimitiveType::Bool)),
            i32_: Ty::intern(db, TypeData::Primitive(ty::PrimitiveType::I32)),
            f32_: Ty::intern(db, TypeData::Primitive(ty::PrimitiveType::F32)),
            stmt: Ty::intern(db, TypeData::Stmt),
            unknown: Ty::intern(db, TypeData::Unknown),
        }
    }
}

/// Visits all expressions and patterns
struct Collect<'db> {
    db: &'db dyn IrDb,
    body_data: &'db BodyData,
    debug: DebugContext<'db>,
    interned: InternedTypes,
    proc: item::Proc,
    // resolver: Resolver,
    // TODO: refer to external items?
    expr_types: FxHashMap<Expr, TyIndex>,
    pat_types: FxHashMap<Pat, TyIndex>,
    types: TiVec<TyIndex, WipTypeData>,
}

struct Infer<'db, 'map> {
    db: &'db dyn IrDb,
    body_data: &'db BodyData,
    // debug: DebugContext<'db>,
    interned: InternedTypes,
    proc: item::Proc,
    // resolver: Resolver,
    // types associated with Expr/Pat are immutable in `Infer` pass
    expr_types: &'map FxHashMap<Expr, TyIndex>,
    pat_types: &'map FxHashMap<Pat, TyIndex>,
    types: TiVec<TyIndex, WipTypeData>,
}

impl<'db> Collect<'db> {
    fn collect_pat_as_var(&mut self, pat: Pat) {
        let pat_data = &self.body_data.tables[pat];

        let wip_ty = match pat_data {
            PatData::Missing => {
                let ty = Ty::intern(self.db, TypeData::Unknown);
                WipTypeData::Ty(ty)
            }
            PatData::Bind { .. } => WipTypeData::Var,
        };

        self.insert_pat_wip_ty(pat, wip_ty);
    }

    fn insert_pat_wip_ty(&mut self, pat: Pat, wip_ty: WipTypeData) {
        let ty_index = self.types.push_and_get_key(wip_ty);

        assert!(
            self.pat_types.insert(pat, ty_index).is_none(),
            "bug: duplicate visit to pat: {:?} {:?}",
            pat,
            self.body_data.tables[pat].debug(&self.debug)
        );
    }

    fn share_expr_type_with_pat(&mut self, expr: Expr, shared: Pat) {
        let ty_index = self.pat_types[&shared];

        assert!(
            self.expr_types.insert(expr, ty_index).is_none(),
            "bug: duplicate visit: {:?}",
            expr.debug(&self.debug),
        );
    }

    fn share_expr_type_with_expr(&mut self, expr: Expr, shared: Expr) {
        let ty_index = self.expr_types[&shared];

        assert!(
            self.expr_types.insert(expr, ty_index).is_none(),
            "bug: duplicate visit: {:?}",
            expr.debug(&self.debug),
        );
    }

    fn share_pat_type_with_expr(&mut self, pat: Pat, shared: Expr) {
        let ty_index = self.expr_types[&shared];

        assert!(
            self.pat_types.insert(pat, ty_index).is_none(),
            "bug: duplicate visit: {:?}",
            pat.debug(&self.debug),
        );
    }

    fn insert_expr_ty(&mut self, expr: Expr, ty: Ty) {
        self.insert_expr_wip_ty(expr, WipTypeData::Ty(ty));
    }

    fn insert_expr_wip_ty(&mut self, expr: Expr, ty: WipTypeData) {
        let index = self.types.push_and_get_key(ty);

        assert!(
            self.expr_types.insert(expr, index).is_none(),
            "bug: duplicate visit of expr: {:?} {:?}",
            expr,
            self.body_data.tables[expr].debug(&self.debug)
        );
    }
}

impl<'db> Collect<'db> {
    pub fn collect(&mut self) {
        self.collect_params();
        self.collect_expr(self.body_data.root_block);
    }

    fn collect_params(&mut self) {
        for &param_pat in &self.body_data.param_pats {
            // FIXME: parse type annotation
            self.insert_pat_wip_ty(param_pat, WipTypeData::Ty(self.interned.i32_));
        }
    }

    fn collect_expr(&mut self, expr: Expr) {
        let expr_data = &self.body_data.tables[expr];

        let wip_ty = match expr_data {
            ExprData::Missing => {
                // TODO: reconsider
                // missing expression can have any type
                WipTypeData::Var
            }

            ExprData::Block(block) => {
                block.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                if let Some(last_expr) = block.exprs.last() {
                    // block has the same type as the last expression
                    self.share_expr_type_with_expr(expr, *last_expr);
                    return;
                } else {
                    // or it returns none
                    let ty = Ty::intern(self.db, TypeData::Stmt);
                    WipTypeData::Ty(ty)
                }
            }
            ExprData::Let(let_) => {
                self.collect_expr(let_.rhs);

                // the binding pattern has the same type as the RHS experssion
                self.share_pat_type_with_expr(let_.pat, let_.rhs);

                WipTypeData::Ty(self.interned.stmt)
            }
            ExprData::Call(call) => {
                let ret_ty = self
                    .collect_user_proc(call.path)
                    .unwrap_or(self.interned.unknown);

                call.args.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                WipTypeData::Ty(ret_ty)
            }
            ExprData::CallOp(op) => {
                op.args.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                self.collect_expr(op.op_expr);

                // return type of the builtin function call needs to be inferred
                // TODO: set type for logical operators here
                WipTypeData::Var
            }
            ExprData::Op(_) => {
                // builtin operator type needs to be inferred
                WipTypeData::Var
            }

            ExprData::Literal(lit) => match lit {
                expr::Literal::String(_) => todo!(),
                expr::Literal::Char(_) => todo!(),
                expr::Literal::Bool(_) => WipTypeData::Ty(self.interned.bool_),
                expr::Literal::F32(_) => WipTypeData::Ty(self.interned.f32_),
                expr::Literal::I32(_) => WipTypeData::Ty(self.interned.i32_),
            },
            // TODO: use name resolution to know the type
            ExprData::Path(path) => {
                if self.resolve_path_as_value(path, expr) {
                    // no new type data
                    return;
                } else {
                    // path to nothing
                    let diag = CannotFindValueInScope { expr };
                    TypeDiagnostics::push(self.db, diag.into());
                    WipTypeData::Ty(self.interned.unknown)
                }
            }
            ExprData::And(and) => {
                and.exprs.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                WipTypeData::Ty(self.interned.bool_)
            }
            ExprData::Or(or) => {
                or.exprs.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                WipTypeData::Ty(self.interned.bool_)
            }
            ExprData::When(when) => {
                self.collect_expr(when.pred);
                self.collect_expr(when.block);

                WipTypeData::Ty(self.interned.stmt)
            }
            ExprData::Unless(unless) => {
                self.collect_expr(unless.pred);
                self.collect_expr(unless.block);

                WipTypeData::Ty(self.interned.stmt)
            }
            ExprData::Cond(cond) => {
                for case in &cond.cases {
                    self.collect_expr(case.pred);
                    self.collect_expr(case.block);
                }

                if cond.can_be_expr {
                    WipTypeData::Var
                } else {
                    WipTypeData::Ty(self.interned.stmt)
                }
            }
            ExprData::Loop(loop_) => {
                self.collect_expr(loop_.block);

                // FIXME: loop can be an expression
                WipTypeData::Ty(self.interned.stmt)
            }
            ExprData::While(while_) => {
                self.collect_expr(while_.pred);
                self.collect_expr(while_.block);

                WipTypeData::Ty(self.interned.stmt)
            }
            ExprData::Set(set) => {
                self.collect_expr(set.place);
                self.collect_expr(set.rhs);

                WipTypeData::Ty(self.interned.stmt)
            }
        };

        self.insert_expr_wip_ty(expr, wip_ty);
    }

    /// Resolves path to a value namespce and point to the same type
    fn resolve_path_as_value(&mut self, path: &expr::Path, expr: Expr) -> bool {
        // FIXME(perf):
        let resolver = self.proc.expr_resolver(self.db, expr);

        match resolver.resolve_path_as_value(self.db, path) {
            Some(ValueNs::Pat(pat)) => {
                self.share_expr_type_with_pat(expr, pat);
                true
            }
            Some(ValueNs::Proc(_proc)) => {
                // FIXME: handle procedure type
                let ty = Ty::intern(self.db, TypeData::Primitive(ty::PrimitiveType::I32));
                let ty_index = self.types.push_and_get_key(WipTypeData::Ty(ty));
                self.expr_types.insert(expr, ty_index);
                true
            }
            None => false,
        }
    }

    /// Returns the procedure's return type if known
    fn collect_user_proc(&mut self, path_expr: Expr) -> Option<Ty> {
        // FIXME(perf):
        let resolver = self.proc.expr_resolver(self.db, path_expr);

        // TODO: resolve procudure and run check
        if let ExprData::Path(path) = &self.body_data.tables[path_expr] {
            if let Some(v) = resolver.resolve_path_as_value(self.db, path) {
                match v {
                    ValueNs::Proc(proc) => {
                        let proc_ty = proc.ty(self.db);
                        self.insert_expr_ty(path_expr, proc_ty);

                        let ret_ty = match proc_ty.data(self.db) {
                            TypeData::Proc(proc_ty) => proc_ty.ret_ty,
                            _ => unreachable!(),
                        };

                        return Some(ret_ty);
                    }
                    ValueNs::Pat(_pat) => {
                        // TODO: call variable as a procedure
                    }
                }
            } else {
                let diag = CannotFindValueInScope { expr: path_expr };
                TypeDiagnostics::push(self.db, diag.into());
            }
        } else {
            // error already emitted?
        }

        // can't resolve path; conclude as unknown type
        let ty_index = self
            .types
            .push_and_get_key(WipTypeData::Ty(self.interned.unknown));
        self.expr_types.insert(path_expr, ty_index);

        None
    }
}

/// # Type inference
/// Invariant: The type inference only assigns types to type variables. Known pattern / expression
/// types are never overwritten.
impl<'db, 'map> Infer<'db, 'map> {
    pub fn infer_all(&mut self) {
        self.infer_expr(self.body_data.root_block)
    }

    /// Unifies expression types with expected types
    fn infer_expr(&mut self, expr: Expr) {
        let expr_data = &self.body_data.tables[expr];

        match expr_data {
            ExprData::Missing => {}
            ExprData::Block(block) => {
                block.iter().for_each(|&expr| {
                    self.infer_expr(expr);
                });

                if let Some(last) = block.exprs.last() {
                    assert_eq!(
                        self.expr_types[&expr], self.expr_types[last],
                        "the block and the last expression must have tthe same type"
                    );
                }
            }
            ExprData::Let(let_) => {
                self.infer_expr(let_.rhs);

                assert_eq!(
                    self.expr_types[&let_.rhs], self.pat_types[&let_.pat],
                    "the binding pattern and the RHS must share the same type"
                );
            }
            ExprData::Call(call) => self.infer_call(expr, call),
            ExprData::CallOp(op) => self.infer_builtin_op(expr, op),
            ExprData::Op(_kind) => {
                // the operator type is unified on visiting `CallOp`
            }

            ExprData::Literal(_lit) => {}
            ExprData::Path(_path) => {
                // path is alread resolve to a pattern and the pattern's type is shared the path
                // expression
            }
            ExprData::And(and) => {
                and.exprs.iter().for_each(|&expr| {
                    self.infer_expr(expr);
                    self.unify_bool_expr(expr);
                });
            }
            ExprData::Or(or) => {
                or.exprs.iter().for_each(|&expr| {
                    self.infer_expr(expr);
                    self.unify_bool_expr(expr);
                });
            }
            ExprData::When(when) => {
                self.infer_expr(when.pred);
                self.unify_bool_expr(when.pred);

                self.infer_expr(when.block);
            }
            ExprData::Unless(unless) => {
                self.infer_expr(unless.pred);
                self.unify_bool_expr(unless.pred);

                self.infer_expr(unless.block);
            }
            ExprData::Cond(cond) => {
                // infer predicates
                for case in &cond.cases {
                    self.infer_expr(case.pred);
                    self.unify_bool_expr(case.pred);
                }

                if !cond.can_be_expr {
                    // infer blocks types, but without unifying them
                    for case in &cond.cases {
                        self.infer_expr(case.block);
                    }
                    return;
                }

                // infer and unify block types
                let exprs = cond.cases.iter().map(|case| case.block).collect::<Vec<_>>();
                exprs.iter().for_each(|e| self.infer_expr(*e));
                if let Some((_expr, _mismatch)) = self.unify_same_types(&exprs) {
                    todo!("report cond case type mismatch");
                }

                // FIXME: Get unified type and use it for the cond's type
                let wip_ty_data = if let Some(case) = cond.cases.first() {
                    let ty_index = self.expr_types[&case.block];
                    self.types[ty_index].clone()
                } else {
                    WipTypeData::Ty(self.interned.stmt)
                };

                let ty_index = self.expr_types[&expr];
                self.types[ty_index] = wip_ty_data;
            }
            ExprData::Loop(loop_) => {
                // TODO: loop expression
                self.infer_expr(loop_.block);
            }
            ExprData::While(while_) => {
                self.infer_expr(while_.pred);
                self.unify_bool_expr(while_.pred);
                self.infer_expr(while_.block);
            }
            ExprData::Set(set) => {
                self.infer_expr(set.place);
                self.infer_expr(set.rhs);
                self.unify_expr_vars(set.place, set.rhs);
            }
        }
    }

    fn infer_builtin_op(&mut self, call_op_expr: Expr, call_op: &expr::CallOp) {
        call_op.args.iter().for_each(|e| self.infer_expr(*e));
        if let Some((first_expr, mismatch)) = self.unify_same_types(&call_op.args) {
            TypeDiagnostics::push(
                self.db,
                IncompatibleOpArgTypes {
                    op_expr: call_op.op_expr,
                    first_expr,
                    mismatch,
                }
                .into(),
            );
        }

        let operand_ty = call_op
            .args
            .iter()
            .find_map(|expr| {
                let index = self.expr_types[expr];
                let ty = &self.types[index];
                let ty_data = match ty {
                    WipTypeData::Var => unreachable!("not infferd type?"),
                    WipTypeData::Ty(ty) => ty.data(self.db),
                };
                ty::OpOperandType::from_type_data(ty_data)
            })
            .unwrap_or(ty::OpOperandType::Unknown);

        if operand_ty == ty::OpOperandType::Unknown {
            // call operator type is unknown
            self.types[self.expr_types[&call_op_expr]] = WipTypeData::Ty(self.interned.unknown);
            // call node type is unknown
            self.types[self.expr_types[&call_op.op_expr]] = WipTypeData::Ty(self.interned.unknown);
            return;
        }

        let kind = match &self.body_data.tables[call_op.op_expr] {
            ExprData::Op(op) => *op,
            _ => unreachable!(),
        };

        // operator type
        self.types[self.expr_types[&call_op.op_expr]] = {
            let op_type = ty::OpType { kind, operand_ty };
            let ty = Ty::intern(self.db, TypeData::Op(op_type));
            WipTypeData::Ty(ty)
        };

        // call type
        self.types[self.expr_types[&call_op_expr]] = {
            if kind.is_bool() {
                // boolean opeartors have boolean types
                WipTypeData::Ty(self.interned.bool_)
            } else {
                // arithmetic operator is of the type of the operand
                let ty_data = match operand_ty.to_type_data() {
                    Some(x) => x,
                    None => {
                        unreachable!("bug: operand type should be known here: {:?}", operand_ty);
                    }
                };

                let ty = Ty::intern(self.db, ty_data);
                WipTypeData::Ty(ty)
            }
        };
    }

    fn infer_call(&mut self, call_expr: Expr, call: &expr::Call) {
        // FIXME(perf):
        let resolver = self.proc.expr_resolver(self.db, call_expr);

        if let ExprData::Path(path) = &self.body_data.tables[call.path] {
            if let Some(v) = resolver.resolve_path_as_value(self.db, path) {
                match v {
                    ValueNs::Proc(proc) => {
                        self.infer_resolved_proc_call(call_expr, call, proc);
                        return;
                    }
                    ValueNs::Pat(_pat) => {
                        todo!("call variable as a procedure")
                    }
                }
            }
        }

        // TODO: diagnostics
        let diag = CannotFindValueInScope { expr: call_expr };
        TypeDiagnostics::push(self.db, diag.into());

        // infer the unresolved procedure call with best effor
        self.infer_expr(call.path);

        call.args.iter().for_each(|expr| {
            self.infer_expr(*expr);
        });
    }

    /// Unifies the argument types with the parameter types. Returns true on success
    fn infer_resolved_proc_call(
        &mut self,
        _call_expr: Expr,
        call: &expr::Call,
        proc: item::Proc,
    ) -> bool {
        // infer
        for arg in call.args.iter() {
            self.infer_expr(*arg);
        }

        let proc_ty = proc.ty_data_as_proc(self.db);

        let mut type_mismatches = Vec::new();

        for i in 0..cmp::min(proc_ty.param_tys.len(), call.args.len()) {
            let param_ty = proc_ty.param_tys[i];
            let arg_expr = call.args[i];

            if let Some(mismatch) = self.unify_expected_expr(arg_expr, param_ty) {
                type_mismatches.push((i, mismatch));
            }
        }

        let arity_match = proc_ty.param_tys.len() == call.args.len();
        let err = !(arity_match && type_mismatches.is_empty());

        if err {
            let arity_mismatch = if arity_match {
                None
            } else {
                Some(ty_diag::ArityMismatch {
                    expected: proc_ty.param_tys.len(),
                    actual: call.args.len(),
                })
            };

            let diag = ty_diag::IncorrectProcArgs {
                proc_expr: call.path,
                proc_id: proc,
                type_mismatches,
                arity_mismatch,
            };

            ty_diag::TypeDiagnostics::push(self.db, diag.into());
        }

        !err
    }
}

impl<'db, 'map> Infer<'db, 'map> {
    // TODO: occur check for procedure types
    // /// Returns true if the type variable occurs in the compared type. This is used in [`unify`] to avoid inifinite call cycle.
    // fn occur(&self, var: TypeId, ty: TypeId) -> bool {
    //     assert_eq!(
    //         var.get(self),
    //         Some(&WipTypeData::Var),
    //         "not a type variable"
    //     );
    //
    //     todo!()
    // }

    pub fn unify_expected_expr(&mut self, expr: Expr, expected_ty: Ty) -> Option<TypeMismatch> {
        let ty_index = self.expr_types[&expr];
        let wip_ty = &self.types[ty_index];

        match wip_ty {
            WipTypeData::Var => {
                self.types[ty_index] = WipTypeData::Ty(expected_ty.clone());
                None
            }
            WipTypeData::Ty(ty) => {
                let ty_data = ty.data(self.db);
                let expected_data = expected_ty.data(self.db);

                // TODO: consider comparing by ID
                if ty_data == expected_data {
                    return None;
                }

                Some(TypeMismatch {
                    expected_ty,
                    actual_expr: expr,
                    actual_ty: *ty,
                })
            }
        }
    }

    /// Returns true on success
    pub fn unify_bool_expr(&mut self, expr: Expr) -> bool {
        let expected_ty = self.interned.bool_;
        if let Some(mismatch) = self.unify_expected_expr(expr, expected_ty) {
            let diag = MismatchedTypes { mismatch };
            TypeDiagnostics::push(self.db, diag.into());
            false
        } else {
            true
        }
    }

    /// Compares two types, tries to assign type to type variables and returns true if they match.
    pub fn unify_expr_vars(&mut self, e1: Expr, e2: Expr) -> bool {
        let i1 = self.expr_types[&e1];
        let i2 = self.expr_types[&e2];

        if i1 == i2 {
            return true;
        }

        let w1 = &self.types[i1];
        let w2 = &self.types[i2];

        // FIXME: do occur check to avoid inifnite loop
        match (w1, w2) {
            (WipTypeData::Var, WipTypeData::Var) => true,
            (WipTypeData::Ty(_), WipTypeData::Var) => {
                // assign to the type type
                let w1 = w1.clone();
                self.types[i2] = w1;
                true
            }
            (WipTypeData::Var, WipTypeData::Ty(_)) => {
                // assign to the type type
                let w2 = w2.clone();
                self.types[i1] = w2;
                true
            }
            (WipTypeData::Ty(t1), WipTypeData::Ty(t2)) => self.cmp_known_expr_tys(e1, *t1, e2, *t2),
        }
    }

    /// Be sure to infer first
    ///
    /// Returns the first type expression and the first mismatch information if any.
    pub fn unify_same_types(&mut self, exprs: &[Expr]) -> Option<(Expr, TypeMismatch)> {
        // the first unknown type of the branches is used as the expected type:
        let (expected_expr, expected_ty) = exprs.iter().find_map(|&expr| {
            let wip_ty = &self.types[self.expr_types[&expr]];

            let ty = match wip_ty {
                WipTypeData::Var => {
                    panic!("not inferred on unification: {:?}", wip_ty.debug(self.db),)
                }
                WipTypeData::Ty(ty) => *ty,
            };

            if ty == self.interned.unknown {
                None
            } else {
                Some((expr, ty))
            }
        })?;

        for &expr in exprs {
            if expr == expected_expr {
                continue;
            }

            let ty_index = self.expr_types[&expr];
            let wip_ty = &self.types[ty_index];
            let ty = *wip_ty.ty().unwrap();

            if ty == self.interned.unknown {
                continue;
            }

            // unify, but without
            if let Some(mismatch) = self.unify_expected_expr(expr, expected_ty) {
                return Some((expected_expr, mismatch));
            }
        }

        None
    }

    /// Compares two known types
    fn cmp_known_expr_tys(&self, e1: Expr, t1: Ty, e2: Expr, t2: Ty) -> bool {
        let i1 = self.expr_types[&e1];
        let i2 = self.expr_types[&e2];

        let ty_data1 = &self.types[i1].cast_as_data(self.db);
        let ty_data2 = &self.types[i2].cast_as_data(self.db);

        let matches = match (ty_data1, ty_data2) {
            (TypeData::Primitive(p1), TypeData::Primitive(p2)) => p1 == p2,
            (TypeData::Op(o1), TypeData::Op(o2)) => o1 == o2,
            _ => false,
        };

        if !matches {
            let diag = MismatchedTypes {
                mismatch: TypeMismatch {
                    expected_ty: t1,
                    actual_expr: e2,
                    actual_ty: t2,
                },
            };
            TypeDiagnostics::push(self.db, diag.into());
        }

        matches
    }
}

#[salsa::tracked(jar = IrJar)]
pub(crate) fn lower_proc_type(db: &dyn IrDb, proc: item::Proc) -> Ty {
    let param_tys = {
        let mut param_tys = Vec::new();

        let resolver = proc.proc_ty_resolver(db);
        for param in proc.params(db).iter() {
            let ty = self::lower_param_ty(db, &resolver, param, &param.ty);
            param_tys.push(ty);
        }

        param_tys.into_boxed_slice()
    };

    let ret_ty = match proc.return_ty(db) {
        Some(ty_syntax) => {
            let resolver = proc.proc_ty_resolver(db);
            self::lower_type_syntax(db, &resolver, ty_syntax)
        }
        None => Ty::intern(db, TypeData::Stmt),
    };

    let proc_ty = ty::ProcType { param_tys, ret_ty };

    Ty::intern(db, TypeData::Proc(proc_ty))
}

fn lower_param_ty(
    db: &dyn IrDb,
    resolver: &Resolver,
    _param: &item::Param,
    ty_syntax: &expr::TypeSyntax,
) -> Ty {
    use expr::TypeSyntax;

    match ty_syntax {
        TypeSyntax::Missing => Ty::intern(db, ty::TypeData::Unknown),
        TypeSyntax::Path(path) => self::lower_type_path(db, resolver, path),
        TypeSyntax::Primitive(prim) => Ty::intern(db, ty::TypeData::Primitive(*prim)),
    }
}

fn lower_type_syntax(db: &dyn IrDb, resolver: &Resolver, ty_syntax: &expr::TypeSyntax) -> Ty {
    use expr::TypeSyntax;

    match ty_syntax {
        TypeSyntax::Missing => {
            // FIXME: report missing type syntax
            Ty::intern(db, ty::TypeData::Unknown)
        }
        TypeSyntax::Path(path) => self::lower_type_path(db, resolver, path),
        TypeSyntax::Primitive(prim) => Ty::intern(db, ty::TypeData::Primitive(*prim)),
    }
}

fn lower_type_path(db: &dyn IrDb, _resolver: &Resolver, path: &expr::Path) -> Ty {
    assert_eq!(path.segments.len(), 1, "TODO: support path");

    let name = path.segments[0].as_str(db.base());
    if let Some(_) = ty::PrimitiveType::parse(name) {
        unreachable!("primitive types are never parsed as `TypeSyntax::Primitive`");
    }

    // TODO: resolve user-defined types

    Ty::intern(db, ty::TypeData::Unknown)
}
