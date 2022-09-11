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

    fn compile_expr(&mut self, db: &Db, body_data: &BodyData, types: &TypeTable, expr: Expr) {
        let expr_data = &body_data.tables[expr];

        match expr_data {
            ExprData::Missing => {
                todo!("{:#?}", expr_data)
            }
            ExprData::Block(block) => {
                // TODO: compile root block directly
                for &expr in block {
                    // TODO: move validation in `validate` path
                    assert_ne!(types[expr], ty::TypeData::Unknown);

                    self.compile_expr(db, &body_data, types, expr);
                }

                // TODO: handle tail expression's return value: return or discard
            }
            ExprData::Call(call) => {
                if let ty::TypeData::Op(op) = &types[call.path] {
                    assert_eq!(
                        call.args.len(),
                        2,
                        "binary operator must take just two arguments"
                    );

                    let lhs_idx = call.args[0];
                    self.compile_expr(db, body_data, types, lhs_idx);

                    let rhs_idx = call.args[1];
                    self.compile_expr(db, body_data, types, rhs_idx);

                    let op = match (op.kind, op.target_ty) {
                        (ty::OpKind::Add, ty::OpTargetType::I32) => Op::AddI32,
                        (ty::OpKind::Add, ty::OpTargetType::F32) => Op::AddF32,
                        (ty::OpKind::Sub, ty::OpTargetType::I32) => Op::SubI32,
                        (ty::OpKind::Sub, ty::OpTargetType::F32) => Op::SubF32,
                        (ty::OpKind::Mul, ty::OpTargetType::I32) => Op::MulI32,
                        (ty::OpKind::Mul, ty::OpTargetType::F32) => Op::MulF32,
                        (ty::OpKind::Div, ty::OpTargetType::I32) => Op::DivI32,
                        (ty::OpKind::Div, ty::OpTargetType::F32) => Op::DivF32,
                        _ => todo!("{:?}", op),
                    };

                    self.chunk.write_code(op);
                } else {
                    // TODO: resolve `Call` to typed builtin methods
                    let path = body_data.tables[call.path].clone().into_path();

                    // TODO: maybe support dot-separated path
                    let name = path.segments[0].clone();

                    todo!("non-builtin function call: {:?}", call);
                }
            }
            ExprData::Let(let_) => {
                let rhs_idx = let_.rhs;
                let rhs = self.compile_expr(db, body_data, types, rhs_idx);

                let local_idx = self.call_frame.as_ref().unwrap().locals[&let_.pat];
                assert!(local_idx <= u8::MAX as usize);

                let local_idx = local_idx as u8;
                self.chunk.write_set_local_u8(local_idx);
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
                let local_idx = self
                    .call_frame
                    .as_ref()
                    .unwrap()
                    .resolve_path(db, expr, path)
                    .unwrap();

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
                self.compile_branch(db, body_data, types, when.pred, when.block, true);
            }
            ExprData::Unless(unless) => {
                self.compile_branch(db, body_data, types, unless.pred, unless.block, false);
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
            self.write_anchor(anchor);
        }
    }

    fn compile_branch(
        &mut self,
        db: &Db,
        body_data: &BodyData,
        types: &TypeTable,
        pred: Option<Expr>,
        block: Expr,
        is_when: bool,
    ) {
        let pred = pred.unwrap_or_else(|| panic!("no predicate in branch"));
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
        self.write_anchor(anchor);
    }

    fn write_anchor(&mut self, anchor: JumpAnchor) {
        let ip = self.chunk.bytes().len();
        assert!(ip <= u16::MAX as usize);
        let ip = ip as u16;

        anchor.write_ip(&mut self.chunk, ip);
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
