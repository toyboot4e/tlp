//! Type lowering

// TODO: Accumulate diagnostics

use rustc_hash::FxHashMap;
use typed_index_collections::TiVec;

use crate::ir::{
    body::{
        expr::{self, Expr, ExprData},
        pat::{Pat, PatData},
        BodyData,
    },
    item,
    ty::{self, TyIndex, TypeData, TypeTable, WipTypeData},
    IrDb, IrJar,
};

#[salsa::tracked(jar = IrJar, return_ref)]
pub(crate) fn lower_body_types(db: &dyn IrDb, proc: item::Proc) -> TypeTable {
    let Collect {
        db,
        body_data,
        proc,
        expr_types,
        pat_types,
        types,
    } = {
        let mut collect = Collect {
            db,
            body_data: proc.body_data(db),
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
            WipTypeData::Var => TypeData::Unknown,
            WipTypeData::Data(data) => data,
        })
        .collect();

    TypeTable {
        expr_types,
        pat_types,
        types,
    }
}

/// Visits all expressions and patterns
struct Collect<'db> {
    db: &'db dyn IrDb,
    body_data: &'db BodyData,
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
    proc: item::Proc,
    // resolver: Resolver,
    // types associated with Expr/Pat are immutable in `Infer` pass
    expr_types: &'map FxHashMap<Expr, TyIndex>,
    pat_types: &'map FxHashMap<Pat, TyIndex>,
    types: TiVec<TyIndex, WipTypeData>,
}

impl<'db> Collect<'db> {
    pub fn collect(&mut self) {
        self.collect_expr(self.body_data.root_block);
    }

    fn collect_expr(&mut self, expr: Expr) {
        let expr_data = &self.body_data.tables[expr];

        let ty = match expr_data {
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
                    let index = self.expr_types[last_expr];

                    assert!(
                        self.expr_types.insert(expr, index).is_none(),
                        "bug: duplicate visit to block"
                    );

                    return;
                } else {
                    // or it returns none
                    WipTypeData::Data(TypeData::Stmt)
                }
            }
            ExprData::Let(let_) => {
                self.collect_expr(let_.rhs);
                self.collect_pat(let_.pat);
                WipTypeData::Data(TypeData::Stmt)
            }
            ExprData::Call(call) => {
                call.args.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });
                self.collect_expr(call.path);
                // FIXME: call's return type
                WipTypeData::Data(TypeData::Stmt)
            }
            ExprData::Literal(lit) => match lit {
                expr::Literal::String(_) => todo!(),
                expr::Literal::Char(_) => todo!(),
                expr::Literal::Bool(_) => {
                    WipTypeData::Data(TypeData::Primitive(ty::PrimitiveType::Bool))
                }
                expr::Literal::F32(_) => {
                    WipTypeData::Data(TypeData::Primitive(ty::PrimitiveType::F32))
                }
                expr::Literal::I32(_) => {
                    WipTypeData::Data(TypeData::Primitive(ty::PrimitiveType::I32))
                }
            },
            // TODO: use name resolution to know the type
            ExprData::Path(path) => {
                if let Some(ty) = self.collect_builtin_op(path) {
                    ty
                } else if self.collect_path(path, expr) {
                    // resolved; there's no new type data
                    return;
                } else {
                    // path to nothing
                    WipTypeData::Data(TypeData::Unknown)
                }
            }
            ExprData::And(and) => {
                and.exprs.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                WipTypeData::Data(TypeData::Primitive(ty::PrimitiveType::Bool))
            }
            ExprData::Or(or) => {
                or.exprs.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                WipTypeData::Data(TypeData::Primitive(ty::PrimitiveType::Bool))
            }
            ExprData::When(when) => {
                self.collect_expr(when.pred);
                self.collect_expr(when.block);

                WipTypeData::Data(TypeData::Stmt)
            }
            ExprData::Unless(unless) => {
                self.collect_expr(unless.pred);
                self.collect_expr(unless.block);

                WipTypeData::Data(TypeData::Stmt)
            }
            ExprData::Cond(cond) => {
                for case in &cond.cases {
                    self.collect_expr(case.pred);
                    self.collect_expr(case.block);
                }

                if cond.can_be_expr {
                    WipTypeData::Var
                } else {
                    WipTypeData::Data(TypeData::Stmt)
                }
            }
            ExprData::Set(set) => {
                self.collect_expr(set.place);
                self.collect_expr(set.rhs);

                WipTypeData::Data(TypeData::Stmt)
            }
        };

        let index = self.types.push_and_get_key(ty);
        assert!(
            self.expr_types.insert(expr, index).is_none(),
            "bug: duplicate visit of expr: {:?} {:?}",
            expr,
            self.body_data.tables[expr]
        );
    }

    /// Builtin operators can be overloaded so they're given type variables
    fn collect_builtin_op(&mut self, path: &expr::Path) -> Option<WipTypeData> {
        if path.segments.len() == 1 {
            let ident = path.segments[0].as_str(self.db.base());
            if let Some(_) = ty::OpKind::parse(ident) {
                return Some(WipTypeData::Var);
            }
        }

        None
    }

    /// Resolves path to a variable and point to the same type
    fn collect_path(&mut self, path: &expr::Path, expr: Expr) -> bool {
        // FIXME(perf)
        let resolver = self.proc.expr_resolver(self.db, expr);

        let pat = match resolver.resolve_path_as_pattern(self.db, path) {
            Some(x) => x,
            None => return false,
        };

        let index = *match self.pat_types.get(&pat) {
            Some(x) => x,
            _ => unreachable!(
                "bug: found a path that can be resolved but not collected: {:?}",
                path, // TODO: debug with DB
            ),
        };

        assert!(
            self.expr_types.insert(expr, index).is_none(),
            "bug: duplicate visit to path: {:?} {:?}",
            expr,
            path
        );

        true
    }

    fn collect_pat(&mut self, pat: Pat) {
        let pat_data = &self.body_data.tables[pat];

        let ty = match pat_data {
            PatData::Missing => WipTypeData::Data(TypeData::Unknown),
            PatData::Bind { .. } => WipTypeData::Var,
        };

        let index = self.types.push_and_get_key(ty);
        assert!(
            self.pat_types.insert(pat, index).is_none(),
            "bug: duplicate visit to pat: {:?} {:?}",
            pat,
            self.body_data.tables[pat]
        );
    }
}

impl<'db, 'map> Infer<'db, 'map> {
    pub fn infer_all(&mut self) {
        self.infer_expr(self.body_data.root_block)
    }

    fn infer_expr(&mut self, expr: Expr) {
        let expr_data = &self.body_data.tables[expr];

        match expr_data {
            ExprData::Missing => {}
            ExprData::Block(block) => {
                block.iter().for_each(|&expr| {
                    self.infer_expr(expr);
                });

                // block has same type as last expression
            }
            ExprData::Let(let_) => {
                self.infer_expr(let_.rhs);

                let rhs_ty = self.expr_types[&let_.rhs].clone();
                let pat_ty = self.pat_types[&let_.pat];
                self.types[pat_ty] = self.types[rhs_ty].clone();
            }
            ExprData::Call(call) => {
                call.args.iter().for_each(|expr| {
                    self.infer_expr(*expr);
                });

                let path_expr = &self.body_data.tables[call.path];

                let ident = {
                    let path = match path_expr {
                        ExprData::Path(path) => path,
                        _ => todo!("call path"),
                    };
                    assert_eq!(path.segments.len(), 1);

                    &path.segments[0]
                };
                let name = ident.as_str(self.db.base());

                if let Some(kind) = ty::OpKind::parse(name) {
                    self.infer_builtin_op(expr, call, kind);
                } else {
                    // TODO: handle user function call
                }
            }
            ExprData::Literal(_lit) => {}
            ExprData::Path(_path) => {
                // path is alread resolve to a pattern and the pattern's type is shared the path
                // expression
            }
            ExprData::And(and) => {
                let b = TypeData::Primitive(ty::PrimitiveType::Bool);
                and.exprs.iter().for_each(|&expr| {
                    self.unify_expected(self.expr_types[&expr], &b);
                });
            }
            ExprData::Or(or) => {
                let b = TypeData::Primitive(ty::PrimitiveType::Bool);
                or.exprs.iter().for_each(|&expr| {
                    self.unify_expected(self.expr_types[&expr], &b);
                });
            }
            ExprData::When(when) => {
                let b = TypeData::Primitive(ty::PrimitiveType::Bool);
                self.unify_expected(self.expr_types[&when.pred], &b);

                self.infer_expr(when.block);
            }
            ExprData::Unless(unless) => {
                let b = TypeData::Primitive(ty::PrimitiveType::Bool);
                self.unify_expected(self.expr_types[&unless.pred], &b);

                self.infer_expr(unless.block);
            }
            ExprData::Cond(cond) => {
                let b = TypeData::Primitive(ty::PrimitiveType::Bool);
                for case in &cond.cases {
                    self.unify_expected(self.expr_types[&case.pred], &b);
                    self.infer_expr(case.block);
                }

                if !cond.can_be_expr {
                    return;
                }

                // unify blocks
                let tys = cond.cases.iter().map(|case| self.expr_types[&case.block]);
                self.unify_many_vars(tys);

                // FIXME: Get unified type and use it for the cond's type
                let ty = if let Some(case) = cond.cases.first() {
                    let ty_index = self.expr_types[&case.block];
                    self.types[ty_index].clone()
                } else {
                    WipTypeData::Data(TypeData::Stmt)
                };

                let ty_index = self.expr_types[&expr];
                self.types[ty_index] = ty;
            }
            ExprData::Set(set) => {
                let i1 = self.expr_types[&set.place];
                let i2 = self.expr_types[&set.rhs];
                self.unify_2vars(i1, i2);
            }
        }
    }

    fn infer_builtin_op(&mut self, call_expr: Expr, call: &expr::Call, kind: ty::OpKind) {
        // handle builtin operators
        let target_ty = call
            .args
            .iter()
            .find_map(|expr| {
                let index = self.expr_types[expr];
                let ty = &self.types[index];
                ty::OpTargetType::from_wip_type(ty)
            })
            .unwrap_or(ty::OpTargetType::Unknown);

        let op_type = ty::OpType { kind, target_ty };

        self.types[self.expr_types[&call.path]] = WipTypeData::Data(TypeData::Op(op_type));
        self.types[self.expr_types[&call_expr]] = target_ty.to_wip_type().unwrap();
    }

    fn infer_pat(&mut self, _pat: Pat, _expr: Expr) {
        //
    }
}

impl<'db, 'map> Infer<'db, 'map> {
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

    pub fn unify_expected(&mut self, i: TyIndex, expected: &TypeData) -> bool {
        let w = &self.types[i];
        match w {
            WipTypeData::Var => {
                self.types[i] = WipTypeData::Data(expected.clone());
                true
            }
            WipTypeData::Data(t) => t == expected,
        }
    }

    /// Compares two types, tries to assign type to type variables and return if they match.
    pub fn unify_2vars(&mut self, i1: TyIndex, i2: TyIndex) -> bool {
        if i1 == i2 {
            return true;
        }

        let w1 = &self.types[i1];
        let w2 = &self.types[i2];

        // FIXME: do occur check to avoid inifnite loop
        match (w1, w2) {
            (WipTypeData::Var, WipTypeData::Var) => true,
            (WipTypeData::Data(_), WipTypeData::Var) => {
                // assign to the type type
                let w1 = w1.clone();
                self.types[i2] = w1;
                true
            }
            (WipTypeData::Var, WipTypeData::Data(_)) => {
                // assign to the type type
                let w2 = w2.clone();
                self.types[i1] = w2;
                true
            }
            (WipTypeData::Data(t1), WipTypeData::Data(t2)) => self.cmp_known(i1, i2),
        }
    }

    // pub fn unify_many_vars(&mut self, tys: &[TyIndex]) -> bool {
    pub fn unify_many_vars(&mut self, mut tys: impl Iterator<Item = TyIndex>) -> bool {
        let mut res = true;

        // window(2)
        let mut t1 = match tys.next() {
            Some(x) => x,
            None => return false,
        };
        let mut t2 = match tys.next() {
            Some(x) => x,
            None => return false,
        };

        loop {
            res &= self.unify_2vars(t1, t2);

            if let Some(t) = tys.next() {
                t1 = t2;
                t2 = t;
            } else {
                break;
            }
        }

        res
    }

    /// Compares two known types
    fn cmp_known(&self, i1: TyIndex, i2: TyIndex) -> bool {
        let t1 = &self.types[i1].cast_as_data();
        let t2 = &self.types[i2].cast_as_data();

        match (t1, t2) {
            (TypeData::Primitive(p1), TypeData::Primitive(p2)) => p1 == p2,
            (TypeData::Op(o1), TypeData::Op(o2)) => o1 == o2,
            _ => false,
        }
    }
}
