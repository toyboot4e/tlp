//! Type lowering

// TODO: refactor with Visitor?

use std::ops;

use rustc_hash::FxHashMap;

use crate::ir::{
    body::{
        expr::{self, Expr, ExprData},
        pat::{Pat, PatData},
        BodyData,
    },
    item,
    ty::{self, TypeData, WipTypeData},
    IrDb,
};

pub fn lower_body(db: &dyn IrDb, proc: item::Proc) -> TypeTable {
    let mut tcx = Tcx {
        db,
        proc,
        body_data: proc.body_data(db),
        expr_types: Default::default(),
        pat_types: Default::default(),
    };

    tcx.collect();
    tcx.infer_all();

    // unwrap all
    let expr_types = tcx
        .expr_types
        .into_iter()
        .map(|(expr, ty)| match ty {
            WipTypeData::Var => unreachable!(
                "bug: type variable remained at expr {:?}: {:?}",
                expr, &tcx.body_data.tables[expr]
            ),
            WipTypeData::Data(data) => (expr, data),
        })
        .collect();

    let pat_types = tcx
        .pat_types
        .into_iter()
        .map(|(expr, ty)| match ty {
            WipTypeData::Var => unreachable!(),
            WipTypeData::Data(data) => (expr, data),
        })
        .collect();

    TypeTable {
        expr_types,
        pat_types,
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeTable {
    expr_types: FxHashMap<Expr, TypeData>,
    pat_types: FxHashMap<Pat, TypeData>,
}

impl ops::Index<Expr> for TypeTable {
    type Output = TypeData;
    fn index(&self, expr: Expr) -> &Self::Output {
        &self.expr_types[&expr]
    }
}

impl ops::Index<Pat> for TypeTable {
    type Output = TypeData;
    fn index(&self, pat: Pat) -> &Self::Output {
        &self.pat_types[&pat]
    }
}

/// Upcast of type owner IDs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeId {
    Expr(Expr),
    Pat(Pat),
    // Proc(item::Proc),
}

impl TypeId {
    pub fn get<'a>(&self, tcx: &'a Tcx) -> Option<&'a WipTypeData> {
        match self {
            TypeId::Expr(expr) => tcx.expr_types.get(&expr),
            TypeId::Pat(pat) => tcx.pat_types.get(&pat),
        }
    }
}

/// Type lowering context
struct Tcx<'db> {
    db: &'db dyn IrDb,
    proc: item::Proc,
    body_data: &'db BodyData,
    // tables
    // vars:
    // TODO: maybe use `TiVec` instead?
    expr_types: FxHashMap<Expr, WipTypeData>,
    pat_types: FxHashMap<Pat, WipTypeData>,
}

impl<'db> Tcx<'db> {
    pub fn collect(&mut self) {
        self.body_data
            .tables
            .exprs
            .all_keys()
            .for_each(|expr| self.collect_expr(expr));
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
                self.collect_pat(let_.pat);
                WipTypeData::Data(TypeData::Stmt)
            }
            ExprData::Call(call) => {
                self.collect_expr(call.path);
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
            ExprData::Path(path) => WipTypeData::Var,
        };

        self.expr_types.insert(expr, ty);
    }

    fn collect_pat(&mut self, pat: Pat) {
        let pat_data = &self.body_data.tables[pat];

        let ty = match pat_data {
            PatData::Missing => TypeData::Unknown,
            PatData::Bind { name } => {
                todo!()
            }
        };
    }
}

impl<'db> Tcx<'db> {
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
            }
            ExprData::Let(let_) => {
                self.infer_pat(let_.pat);
            }
            ExprData::Call(call) => {
                call.args.iter().for_each(|expr| {
                    self.infer_expr(*expr);
                });

                let path_expr = &self.body_data.tables[call.path];
                let path = match path_expr {
                    ExprData::Path(path) => path,
                    _ => todo!("call path"),
                };
                assert_eq!(path.segments.len(), 1);

                let ident = &path.segments[0];
                let name = ident.as_str(self.db.base());

                if let Some(kind) = ty::OpKind::parse(name) {
                    let target_ty = call
                        .args
                        .iter()
                        .find_map(|expr| {
                            let ty = &self.expr_types[expr];
                            ty::OpTargetType::from_wip_type(ty)
                        })
                        .unwrap_or(ty::OpTargetType::Unknown);

                    let op_type = ty::OpType { kind, target_ty };

                    self.expr_types
                        .insert(call.path, WipTypeData::Data(TypeData::Op(op_type)));
                } else {
                    // TODO: handle user function call
                }
            }
            ExprData::Literal(_lit) => {}
            ExprData::Path(_path) => {
                // TODO: use name resolution to know the type
            }
        }

        self.unwrap_expr_ty(expr);
    }

    fn infer_pat(&mut self, pat: Pat) {
        self.unwrap_pat_ty(pat);
    }

    fn unwrap_expr_ty(&mut self, expr: Expr) {
        let ty = self.expr_types.get_mut(&expr).unwrap();
        if matches!(ty, WipTypeData::Var) {
            *ty = WipTypeData::Data(TypeData::Unknown);
        }
    }

    fn unwrap_pat_ty(&mut self, pat: Pat) {
        let ty = self.pat_types.get_mut(&pat).unwrap();
        if matches!(ty, WipTypeData::Var) {
            *ty = WipTypeData::Data(TypeData::Unknown);
        }
    }
}

impl<'db> Tcx<'db> {
    /// Returns true if the type variable occurs in the compared type. This is used in [`unify`] to avoid inifinite call cycle.
    fn occur(&self, var: TypeId, ty: TypeId) -> bool {
        assert_eq!(
            var.get(self),
            Some(&WipTypeData::Var),
            "not a type variable"
        );

        todo!()
    }

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
