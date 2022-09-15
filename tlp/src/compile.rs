//! Compiler (HIR → bytecode)

pub mod scope;

use rustc_hash::FxHashMap;
use thiserror::Error;

use base::jar::InputFile;

use crate::{
    ir::{
        body::{
            expr::{self, Expr, ExprData},
            pat::{Pat, PatData},
            BodyData,
        },
        item,
        ty::{self, TypeTable},
    },
    vm::code::{Chunk, JumpAnchor, Op, TypedLiteral},
    Db,
};

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("unexpected expr: {expr:?}")]
    UnexpectedExpr { expr: Expr },
    #[error("unexisting method call")]
    UnexistingMethodCall,
    #[error("can't resolve patern with name `{name}`")]
    CantResolvePattern { name: String },
}

pub fn compile(db: &Db, krate: InputFile) -> (Chunk, Vec<CompileError>) {
    let mut compiler = Compiler::default();
    compiler.compile_crate(db, krate);
    (compiler.chunk, compiler.errs)
}

/// Compile-time call frame information
#[derive(Debug)]
struct CallFrame {
    proc: item::Proc,
    // TODO: consider using Vec-based map
    locals: FxHashMap<Pat, usize>,
    locals_capacity: usize,
}

impl CallFrame {
    pub fn new(db: &Db, proc: item::Proc) -> Self {
        let body = proc.body(db);
        let body_data = body.data(db);

        let pats = &body_data.tables.pats;
        let bind_pats = pats
            .enumerate()
            .filter(|(_, data)| matches!(data, PatData::Bind { .. }))
            .map(|(pat, _)| pat);

        let mut locals = FxHashMap::default();
        let mut locals_capacity = 0;

        for (pat, data) in bind_pats.enumerate() {
            locals.insert(data, pat);
            locals_capacity += 1;
        }

        Self {
            proc,
            locals,
            locals_capacity,
        }
    }

    /// Resolves pattern to local variable offset
    pub fn resolve_path(&self, db: &Db, path_expr: Expr, path: &expr::Path) -> Option<usize> {
        // FIXME: solve path
        let name = &path.segments[0];

        let binding_pat = {
            let expr_scopes = self.proc.expr_scopes(db).data(db);
            let scope_id = expr_scopes.scope_for_expr(path_expr)?;
            let entry = expr_scopes.resolve_name_in_scope_chain(scope_id, *name)?;

            entry.pat
        };

        self.locals.get(&binding_pat).map(|x| *x)
    }
}

#[derive(Debug, Default)]
struct Compiler {
    chunk: Chunk,
    errs: Vec<CompileError>,
    /// Current function's call frame information
    call_frame: Option<CallFrame>,
}

impl Compiler {
    pub fn compile_crate(&mut self, db: &Db, krate: InputFile) {
        let main_proc = self::find_procedure_by_name(db, krate, "main");
        self.compile_proc(db, main_proc);
    }

    #[allow(unused)]
    fn compile_proc(&mut self, db: &Db, proc: item::Proc) {
        let body = proc.body(db);
        let body_data = body.data(db);

        // push frame
        {
            let call_frame = CallFrame::new(db, proc);

            // FIXME:
            self.chunk
                .write_alloc_locals_u8(call_frame.locals_capacity as u8);
            self.call_frame = Some(call_frame);
        }

        // body
        {
            let types = proc.type_table(db);
            self.compile_expr(db, body_data, types, body_data.root_block);
        }

        // pop frame
        // FIXME: Handle frame stack pop and return value
        self.chunk.write_code(Op::Ret);
    }

    /// Compiles an expression. It returns some value or `<none>`
    fn compile_expr(&mut self, db: &Db, body_data: &BodyData, types: &TypeTable, expr: Expr) {
        let expr_data = &body_data.tables[expr];

        match expr_data {
            ExprData::Missing => {
                todo!("{:#?}", expr_data)
            }
            ExprData::Block(block) => {
                let (last, exprs) = match block.exprs.split_last() {
                    Some(x) => x,
                    None => {
                        self.chunk.write_code(Op::PushNone);
                        return;
                    }
                };

                for &expr in exprs {
                    // TODO: move validation in `validate` path
                    assert_ne!(types[expr], ty::TypeData::Unknown);

                    // return values other than the last one are discarded
                    self.compile_expr(db, &body_data, types, expr);
                    self.chunk.write_code(Op::Discard);
                }

                self.compile_expr(db, &body_data, types, *last);

                // TODO: handle tail expression's return value: return or discard
            }
            ExprData::Call(call) => {
                // let path = body_data.tables[call.path].clone().into_path();
                let path = body_data.tables[call.path].cast_as_path();

                // TODO: maybe support dot-separated path
                let name = path.segments[0].clone();

                todo!("non-builtin function call: {:?}", call);
            }
            ExprData::CallOp(call_op) => {
                assert_eq!(
                    call_op.args.len(),
                    2,
                    "operator must take just two arguments (for now)"
                );

                let lhs_idx = call_op.args[0];
                self.compile_expr(db, body_data, types, lhs_idx);

                let rhs_idx = call_op.args[1];
                self.compile_expr(db, body_data, types, rhs_idx);

                let op_ty = match &types[call_op.op_expr] {
                    ty::TypeData::Op(op_ty) => op_ty,
                    x => unreachable!("not operator type: {:?} for operator {:?}", x, call_op),
                };

                let op = match (op_ty.kind, op_ty.operand_ty) {
                    (expr::OpKind::Add, ty::OpOperandType::I32) => Op::AddI32,
                    (expr::OpKind::Add, ty::OpOperandType::F32) => Op::AddF32,

                    (expr::OpKind::Sub, ty::OpOperandType::I32) => Op::SubI32,
                    (expr::OpKind::Sub, ty::OpOperandType::F32) => Op::SubF32,

                    (expr::OpKind::Mul, ty::OpOperandType::I32) => Op::MulI32,
                    (expr::OpKind::Mul, ty::OpOperandType::F32) => Op::MulF32,

                    (expr::OpKind::Div, ty::OpOperandType::I32) => Op::DivI32,
                    (expr::OpKind::Div, ty::OpOperandType::F32) => Op::DivF32,

                    // =
                    (expr::OpKind::Eq, ty::OpOperandType::Bool) => Op::EqBool,
                    (expr::OpKind::Eq, ty::OpOperandType::I32) => Op::EqI32,
                    (expr::OpKind::Eq, ty::OpOperandType::F32) => Op::EqF32,

                    // !=
                    (expr::OpKind::NotEq, ty::OpOperandType::Bool) => Op::NotEqBool,
                    (expr::OpKind::NotEq, ty::OpOperandType::I32) => Op::NotEqI32,
                    (expr::OpKind::NotEq, ty::OpOperandType::F32) => Op::NotEqF32,

                    // <
                    (expr::OpKind::Lt, ty::OpOperandType::I32) => Op::LtI32,
                    (expr::OpKind::Lt, ty::OpOperandType::F32) => Op::LtF32,

                    // <=
                    (expr::OpKind::Le, ty::OpOperandType::I32) => Op::LeI32,
                    (expr::OpKind::Le, ty::OpOperandType::F32) => Op::LeF32,

                    // >
                    (expr::OpKind::Gt, ty::OpOperandType::I32) => Op::GtI32,
                    (expr::OpKind::Gt, ty::OpOperandType::F32) => Op::GtF32,

                    // >=
                    (expr::OpKind::Ge, ty::OpOperandType::I32) => Op::GeI32,
                    (expr::OpKind::Ge, ty::OpOperandType::F32) => Op::GeF32,

                    _ => todo!("{:?}", call_op),
                };

                self.chunk.write_code(op);
            }
            ExprData::Op(op) => {
                todo!()
            }

            ExprData::Let(let_) => {
                let rhs_idx = let_.rhs;
                let rhs = self.compile_expr(db, body_data, types, rhs_idx);

                let local_idx = self.call_frame.as_ref().unwrap().locals[&let_.pat];
                assert!(local_idx <= u8::MAX as usize);

                let local_idx = local_idx as u8;
                self.chunk.write_set_local_u8(local_idx);

                // push zero as return value
                self.chunk.write_code(Op::PushNone)
            }
            ExprData::Literal(lit) => match lit {
                expr::Literal::F32(x) => {
                    let literal = TypedLiteral::F32(x.0);
                    let idx = self.chunk.store_literal(literal);
                    self.chunk.write_ix(idx);
                }
                expr::Literal::I32(x) => {
                    let literal = TypedLiteral::I32(*x);
                    let idx = self.chunk.store_literal(literal);
                    self.chunk.write_ix(idx);
                }
                expr::Literal::Bool(b) => {
                    let literal = TypedLiteral::Bool(*b);
                    let idx = self.chunk.store_literal(literal);
                    self.chunk.write_ix(idx);
                }
                _ => todo!("{:?}", lit),
            },
            ExprData::Path(path) => {
                let local_idx = self.resolve_path_as_local_index(db, body_data, types, expr, path);
                self.chunk.write_push_local_u8(local_idx as u8);
            }
            ExprData::And(and) => {
                //
                self.compile_bool_oper(db, body_data, types, &and.exprs, true);
            }
            ExprData::Or(or) => {
                //
                self.compile_bool_oper(db, body_data, types, &or.exprs, false);
            }
            ExprData::When(when) => {
                let anchor = self.compile_branch(db, body_data, types, when.pred, when.block, true);

                // discard the last value on `true`
                self.chunk.write_code(Op::Discard);

                anchor.set_ip(&mut self.chunk);
                // `<none>` is the return value of `when` or `unless`:
                self.chunk.write_code(Op::PushNone);
            }
            ExprData::Unless(unless) => {
                let anchor =
                    self.compile_branch(db, body_data, types, unless.pred, unless.block, false);

                // discard the last value on `false`
                self.chunk.write_code(Op::Discard);

                anchor.set_ip(&mut self.chunk);
                // `<none>` is the return value of `when` or `unless`:
                self.chunk.write_code(Op::PushNone);
            }
            ExprData::Cond(cond) => {
                // anchors for the end of each case
                let mut case_end_anchors = Vec::new();

                for case in &cond.cases {
                    let on_mismatch =
                        self.compile_branch(db, body_data, types, case.pred, case.block, true);

                    // go to the end of `cond` on match
                    case_end_anchors.push(self.chunk.write_jump_u16());

                    // go to next cond case on mismatch
                    on_mismatch.set_ip(&mut self.chunk);
                }

                // otherwise: push `<none>`. the case is never reached on `cond` expression
                if !cond.can_be_expr {
                    self.chunk.write_code(Op::PushNone);
                }

                // jump to IP after `cond` on each end of case
                for anchor in case_end_anchors {
                    anchor.set_ip(&mut self.chunk);
                }

                // if it's a statement, overwrite the last expression of any type
                if !cond.can_be_expr {
                    self.chunk.write_code(Op::Discard);
                    self.chunk.write_code(Op::PushNone);
                }
            }
            ExprData::Loop(loop_) => {
                // TODO: handle break
                todo!()
            }
            ExprData::While(while_) => {
                let start = self.chunk.ip();

                self.compile_expr(db, body_data, types, while_.pred);
                let to_end = self.chunk.write_jump_if_not_u16();

                self.compile_expr(db, body_data, types, while_.block);
                self.chunk.write_code(Op::Discard);

                let to_start = self.chunk.write_jump_u16();
                to_start.set_ip_at(&mut self.chunk, start);

                // return value is `<none>`
                to_end.set_ip(&mut self.chunk);
                self.chunk.write_code(Op::PushNone);
            }
            ExprData::Set(set) => {
                self.compile_expr(db, body_data, types, set.rhs);
                let local_idx = self.resolve_expr_as_local_index(db, body_data, types, set.place);
                self.chunk.write_set_local_u8(local_idx as u8);

                // return value is `<none>`
                self.chunk.write_code(Op::PushNone);
            }
        }
    }

    /// `and` or `or`
    fn compile_bool_oper(
        &mut self,
        db: &Db,
        body_data: &BodyData,
        types: &TypeTable,
        exprs: &[Expr],
        is_and: bool,
    ) {
        let mut anchors = Vec::new();

        if is_and {
            // `false` on short-circuit
            self.chunk.write_code(Op::PushFalse);
        } else {
            // `true` on short-circuit
            self.chunk.write_code(Op::PushTrue);
        }

        for &expr in exprs {
            let expr_data = &body_data.tables[expr];

            assert_eq!(
                types[expr],
                ty::TypeData::Primitive(ty::PrimitiveType::Bool)
            );

            self.compile_expr(db, body_data, types, expr);

            let anchor = if is_and {
                // `and`: short circuit on `false`
                self.chunk.write_jump_if_not_u16()
            } else {
                // `or`: short circuit on `true`
                self.chunk.write_jump_if_u16()
            };

            anchors.push(anchor);
        }

        self.chunk.write_code(Op::Discard);

        if is_and {
            self.chunk.write_code(Op::PushTrue);
        } else {
            self.chunk.write_code(Op::PushFalse);
        }

        for anchor in anchors {
            anchor.set_ip(&mut self.chunk);
        }
    }

    /// Compiles a branch without discarding the last value
    fn compile_branch(
        &mut self,
        db: &Db,
        body_data: &BodyData,
        types: &TypeTable,
        pred: Expr,
        block: Expr,
        is_when: bool,
    ) -> JumpAnchor {
        assert_eq!(
            types[pred],
            ty::TypeData::Primitive(ty::PrimitiveType::Bool)
        );

        self.compile_expr(db, body_data, types, pred);

        let anchor = if is_when {
            self.chunk.write_jump_if_not_u16()
        } else {
            self.chunk.write_jump_if_u16()
        };

        self.compile_expr(db, body_data, types, block);

        anchor
    }

    fn resolve_expr_as_local_index(
        &mut self,
        db: &Db,
        body_data: &BodyData,
        types: &TypeTable,
        expr: Expr,
    ) -> usize {
        let path = match &body_data.tables[expr] {
            ExprData::Path(p) => p,
            x => panic!("not a place: {:?}", x),
        };

        self.resolve_path_as_local_index(db, body_data, types, expr, path)
    }

    fn resolve_path_as_local_index(
        &mut self,
        db: &Db,
        body_data: &BodyData,
        types: &TypeTable,
        expr: Expr,
        path: &expr::Path,
    ) -> usize {
        let local_idx = self
            .call_frame
            .as_ref()
            .unwrap()
            .resolve_path(db, expr, path)
            .unwrap_or_else(|| panic!("can't resolve as place: {:?}", path));

        local_idx
    }
}

fn find_procedure_by_name(db: &Db, file: InputFile, name: &str) -> item::Proc {
    let item = db
        .items(file)
        .iter()
        .find(|f| f.name(db).as_str(db) == name)
        .unwrap()
        .clone();

    match item {
        item::Item::Proc(x) => x,
        _ => panic!(),
    }
}
