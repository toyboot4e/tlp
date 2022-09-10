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
    resolve::Resolver,
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

struct Collect<'db> {
    db: &'db dyn IrDb,
    body_data: &'db BodyData,
    proc: item::Proc,
    // resolver: Resolver,
    expr_types: FxHashMap<Expr, TyIndex>,
    pat_types: FxHashMap<Pat, TyIndex>,
    types: TiVec<TyIndex, WipTypeData>,
}

struct Infer<'db, 'map> {
    db: &'db dyn IrDb,
    body_data: &'db BodyData,
    proc: item::Proc,
    // resolver: Resolver,
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
            ExprData::Missing => WipTypeData::Data(TypeData::Unknown),
            ExprData::Block(block) => {
                block.iter().for_each(|&expr| {
                    self.collect_expr(expr);
                });

                // TODO: take the last expression's type
                WipTypeData::Data(TypeData::Stmt)
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
                expr::Literal::Bool(_) => todo!(),
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

    fn collect_path(&mut self, path: &expr::Path, expr: Expr) -> bool {
        // path to variable
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
            "bug: duplicate visit of path: {:?} {:?}",
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
        assert!(self.pat_types.insert(pat, index).is_none(),
            "bug: duplicate visit of pat: {:?} {:?}",
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
                // TODO: switch resolver to get into the block item scope
                block.iter().for_each(|&expr| {
                    self.infer_expr(expr);
                });
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

    /// Compares two types, tries to assign type to type variables and return if they match.
    pub fn unify(&mut self, w1: &WipTypeData, w2: &WipTypeData) -> bool {
        // FIXME: do occur check to avoid inifnite loop
        match (w1, w2) {
            (WipTypeData::Var, WipTypeData::Var) => todo!(),
            (WipTypeData::Data(t1), WipTypeData::Var) => todo!(),
            (WipTypeData::Var, WipTypeData::Data(t2)) => todo!(),
            (WipTypeData::Data(t1), WipTypeData::Data(t2)) => self.cmp_known(t1, t2),
        }
    }

    // assert_eq!(path.segments.len(), 1, "handle path");
    // let ident = path.segments[0];
    // let name = ident.as_str(self.db.as_base_db());

    /// Compares two known types
    fn cmp_known(&mut self, t1: &TypeData, t2: &TypeData) -> bool {
        match (t1, t2) {
            (TypeData::Primitive(p1), TypeData::Primitive(p2)) => self.cmp_primitive(p1, p2),
            (TypeData::Op(o1), TypeData::Op(o2)) => self.cmp_builtin_binary_op(o1, o2),
            _ => false,
        }
    }

    fn cmp_primitive(&mut self, p1: &ty::PrimitiveType, p2: &ty::PrimitiveType) -> bool {
        todo!()
    }

    fn cmp_builtin_binary_op(&mut self, o1: &ty::OpType, o2: &ty::OpType) -> bool {
        todo!()
    }
}
