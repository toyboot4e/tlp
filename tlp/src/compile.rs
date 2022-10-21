//! Compiler (IR → bytecode)

use rustc_hash::FxHashMap;
use salsa::DebugWithDb;
use thiserror::Error;
use typed_index_collections::TiVec;

use base::jar::InputFile;

use crate::{
    ir::{
        body::{
            expr::{self, Expr, ExprData},
            pat::{Pat, PatData},
            BodyData,
        },
        item,
        resolve::ValueNs,
        ty, InputFileExt,
    },
    vm::{
        self,
        code::{Chunk, JumpAnchor, Op, TypedLiteral},
        Vm,
    },
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

pub fn compile_file(db: &Db, main_file: InputFile) -> (Vm, Vec<CompileError>) {
    let mut vm_procs = TiVec::new();
    let mut vm_errs = Vec::new();

    // let main_proc = self::find_procedure_by_name(db, main_file, "main");

    let parse = main_file.parse_items(db);

    let lex_errors = parse.lex_errors(db);

    for err in lex_errors {
        println!("{}", err.render(db, main_file));
        panic!("lexical error");
    }

    let parse_errors = parse.errors(db);

    for err in parse_errors {
        println!("{}", err.render(db, main_file));
    }

    let item_errors = main_file.item_syntax_diags(db);
    for err in &item_errors {
        println!("{}", err.render(db, main_file));
    }

    let items = parse.items(db);

    let proc_ids = items
        .iter()
        .enumerate()
        .filter_map(|(i, item)| match item {
            item::Item::Proc(ir_proc) => {
                let vm_proc_id = vm::VmProcId(i);
                Some((*ir_proc, vm_proc_id))
            }
        })
        .collect::<FxHashMap<_, _>>();

    let mut any_error = !(parse_errors.is_empty() && item_errors.is_empty());

    for item in items {
        // TODO: use validated IR
        // FIXME: consider item declaration diagnostics
        let proc = match item {
            item::Item::Proc(proc) => proc,
        };

        // FIXME: consider procedure body diagnostics

        {
            let mut any_item_error = false;

            // FIXME: `#[return_ref]`?
            let diags = proc.param_ty_diags(db);
            if !diags.is_empty() {
                any_item_error = true;
                crate::ir::ty::ty_diag::eprint_many(db, &diags, main_file, *proc);
            }

            let diags = proc.body_ty_diags(db);
            if !diags.is_empty() {
                any_item_error = true;
                crate::ir::ty::ty_diag::eprint_many(db, &diags, main_file, *proc);
            }

            if any_item_error {
                any_error = true;
                continue;
            }
        }

        let mut compiler = CompileProc::new(&proc_ids, db, proc.clone());
        compiler.compile_proc();

        any_error |= !compiler.errs.is_empty();

        vm_procs.push(vm::VmProc {
            chunk: compiler.chunk,
            n_args: proc.body_data(db).param_pats.len(),
        });
        vm_errs.extend(compiler.errs);
    }

    if any_error {
        panic!("<error>");
    }

    let vm = Vm::new(vm_procs);
    (vm, vm_errs)
}

/// Compile-time call frame information
#[derive(Debug)]
struct CallFrame {
    proc: item::Proc,
    /// Maps [`Pat`] to local index in the call frame
    // TODO: consider using Vec-based map
    locals: FxHashMap<Pat, usize>,
    /// Number of arguments
    #[allow(unused)]
    n_args: usize,
    /// Number of local variables without arguments
    n_locals: usize,
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
        let mut n_vars = 0;

        // REMARK: Arguments must first apear in the binding patterns
        for pat in bind_pats {
            locals.insert(pat, n_vars);
            n_vars += 1;
        }

        let n_args = body_data.param_pats.len();
        let n_locals = n_vars - n_args;

        Self {
            proc,
            locals,
            n_args,
            n_locals,
        }
    }

    pub fn n_vars(&self) -> usize {
        self.n_args + self.n_locals
    }

    /// Resolves pattern to a local variable offset
    pub fn resolve_path_as_local(
        &self,
        db: &Db,
        path_expr: Expr,
        path: &expr::Path,
    ) -> Option<usize> {
        let resolver = self.proc.expr_resolver(db, path_expr);

        let binding_pat = match resolver.resolve_path_as_value(db, path)? {
            ValueNs::Pat(p) => p,
            x => todo!(
                "path was resolved to a non-pattern: path `{:?}`, resolution: `{:?}`",
                path.debug(db),
                x
            ),
        };

        self.locals.get(&binding_pat).map(|x| *x)
    }
}

struct CompileProc<'db> {
    chunk: Chunk,
    errs: Vec<CompileError>,
    /// Current function's call frame information
    call_frame: Option<CallFrame>,
    // FIXME: lifetime
    proc_ids: &'db FxHashMap<item::Proc, vm::VmProcId>,
    // context
    db: &'db Db,
    proc: item::Proc,
    body_data: &'db BodyData,
    types: &'db ty::TypeTable,
}

impl<'db> CompileProc<'db> {
    fn new(
        proc_ids: &'db FxHashMap<item::Proc, vm::VmProcId>,
        db: &'db Db,
        proc: item::Proc,
    ) -> Self {
        let body = proc.body(db);
        let body_data = body.data(db);
        let types = proc.type_table(db);

        Self {
            proc_ids,
            chunk: Default::default(),
            errs: Default::default(),
            call_frame: Default::default(),
            db,
            proc,
            body_data,
            types,
        }
    }

    #[allow(unused)]
    pub fn compile_proc(&mut self) {
        // push the initial call frame frame
        {
            let call_frame = CallFrame::new(self.db, self.proc);
            self.chunk.write_alloc_locals_u8(call_frame.n_locals as u8);
            self.call_frame = Some(call_frame);

            let shift = self.body_data.param_pats.len();
            self.chunk.write_shift_back_u8(shift as u8);
        }

        // body
        self.compile_expr(self.body_data.root_block);

        // pop frame
        self.chunk.write_code(Op::Ret);

        // TODO: Handle frame stack pop and return value
    }

    /// Compiles an expression. It returns some value or `<none>`
    fn compile_expr(&mut self, expr: Expr) {
        let expr_data = &self.body_data.tables[expr];

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
                    assert_ne!(self.types[expr].data(self.db), &ty::TypeData::Unknown);

                    // return values other than the last one are discarded
                    self.compile_expr(expr);
                    self.chunk.write_code(Op::Discard);
                }

                self.compile_expr(*last);

                // TODO: handle tail expression's return value: return or discard
            }
            ExprData::Call(call) => {
                for arg in call.args.iter() {
                    self.compile_expr(*arg);
                }

                self.compile_proc_call_code(call);
            }
            ExprData::CallOp(call_op) => {
                assert_eq!(
                    call_op.args.len(),
                    2,
                    "operator must take just two arguments (for now)"
                );

                let lhs_idx = call_op.args[0];
                self.compile_expr(lhs_idx);

                let rhs_idx = call_op.args[1];
                self.compile_expr(rhs_idx);

                let op_ty = match &self.types[call_op.op_expr].data(self.db) {
                    ty::TypeData::Op(op_ty) => op_ty,
                    x => unreachable!(
                        "not operator type: {:?} for operator {:?}",
                        x,
                        call_op.op_expr.debug(&self.proc.with_db(self.db))
                    ),
                };

                let op = self::to_vm_op(op_ty.clone());
                self.chunk.write_code(op);
            }
            ExprData::Op(_op) => {
                todo!()
            }

            ExprData::Let(let_) => {
                self.compile_expr(let_.rhs);

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
                let local_idx = self.resolve_path_as_local_index(expr, path);
                self.chunk.write_push_local_u8(local_idx as u8);
            }
            ExprData::And(and) => {
                //
                self.compile_bool_oper(&and.exprs, true);
            }
            ExprData::Or(or) => {
                //
                self.compile_bool_oper(&or.exprs, false);
            }
            ExprData::When(when) => {
                let anchor = self.compile_branch(when.pred, when.block, true);

                // discard the last value on `true`
                self.chunk.write_code(Op::Discard);

                anchor.set_ip(&mut self.chunk);
                // `<none>` is the return value of `when` or `unless`:
                self.chunk.write_code(Op::PushNone);
            }
            ExprData::Unless(unless) => {
                let anchor = self.compile_branch(unless.pred, unless.block, false);

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
                    let on_mismatch = self.compile_branch(case.pred, case.block, true);

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
            ExprData::Loop(_loop) => {
                // TODO: handle break
                todo!()
            }
            ExprData::While(while_) => {
                let start = self.chunk.ip();

                self.compile_expr(while_.pred);
                let to_end = self.chunk.write_jump_if_not_u16();

                self.compile_expr(while_.block);
                self.chunk.write_code(Op::Discard);

                let to_start = self.chunk.write_jump_u16();
                to_start.set_ip_at(&mut self.chunk, start);

                // return value is `<none>`
                to_end.set_ip(&mut self.chunk);
                self.chunk.write_code(Op::PushNone);
            }
            ExprData::Set(set) => {
                self.compile_expr(set.rhs);
                let local_idx = self.resolve_expr_as_local_index(set.place);
                self.chunk.write_set_local_u8(local_idx as u8);

                // return value is `<none>`
                self.chunk.write_code(Op::PushNone);
            }
        }
    }

    /// `and` or `or`
    fn compile_bool_oper(&mut self, exprs: &[Expr], is_and: bool) {
        let mut anchors = Vec::new();

        if is_and {
            // `false` on short-circuit
            self.chunk.write_code(Op::PushFalse);
        } else {
            // `true` on short-circuit
            self.chunk.write_code(Op::PushTrue);
        }

        for &expr in exprs {
            assert_eq!(
                self.types[expr].data(self.db),
                &ty::TypeData::Primitive(ty::PrimitiveType::Bool)
            );

            self.compile_expr(expr);

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
    fn compile_branch(&mut self, pred: Expr, block: Expr, is_when: bool) -> JumpAnchor {
        assert_eq!(
            self.types[pred].data(self.db),
            &ty::TypeData::Primitive(ty::PrimitiveType::Bool),
            "not a boolean value: predicate `{:?}`",
            pred,
        );

        self.compile_expr(pred);

        let anchor = if is_when {
            self.chunk.write_jump_if_not_u16()
        } else {
            self.chunk.write_jump_if_u16()
        };

        self.compile_expr(block);

        anchor
    }

    fn resolve_expr_as_local_index(&mut self, expr: Expr) -> usize {
        let path = match &self.body_data.tables[expr] {
            ExprData::Path(p) => p,
            x => panic!("not a place: {:?}", x),
        };

        self.resolve_path_as_local_index(expr, path)
    }

    // When creating a call frame, each pattern is already resolved the their originating binding
    // pattern
    fn resolve_path_as_local_index(&mut self, expr: Expr, path: &expr::Path) -> usize {
        let call_frame = self.call_frame.as_ref().unwrap();
        let local_idx = call_frame
            .resolve_path_as_local(self.db, expr, path)
            .unwrap_or_else(|| {
                panic!(
                    "can't resolve path as a place: expr: {:?}, path: `{:?}`, CallFrame {:?}",
                    expr,
                    path.debug(self.db),
                    call_frame
                )
            });

        local_idx
    }

    fn compile_proc_call_code(&mut self, call: &expr::Call) {
        let path = self.body_data.tables[call.path].cast_as_path();

        assert_eq!(
            path.segments.len(),
            1,
            "TODO: maybe support dot-separated path"
        );

        // FIXME(pref): use name-resolved IR for compilation
        let resolver = self.proc.expr_resolver(self.db, call.path);

        let ir_proc = match resolver
            .resolve_path_as_value(self.db, path)
            .unwrap_or_else(|| panic!("no value for path: `{:?}`", path.debug(self.db)))
        {
            ValueNs::Proc(proc) => proc,
            _ => panic!("not a procedure: {:?}", path.debug(self.db)),
        };

        let vm_proc = self.proc_ids[&ir_proc];
        self.chunk.write_call_proc_u16(vm_proc);
    }
}

fn to_vm_op(op_ty: ty::OpType) -> Op {
    match (op_ty.kind, op_ty.operand_ty) {
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

        _ => todo!("{:?}", op_ty),
    }
}
