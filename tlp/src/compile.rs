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
    vm::code::{Chunk, Op, TypedLiteral},
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

            for &expr in body_data.root_block() {
                self.compile_expr(db, &body_data, types, expr);
            }
        }

        // pop frame
        // FIXME: Handle frame stack pop and return value
        self.chunk.write_code(Op::Ret);
    }

    #[allow(unused)]
    fn compile_expr(&mut self, db: &Db, body_data: &BodyData, types: &TypeTable, expr: Expr) {
        let expr_data = &body_data.tables[expr];

        match expr_data {
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
                _ => todo!(),
            },
            ExprData::Path(path) => {
                let local_idx = self
                    .call_frame
                    .as_ref()
                    .unwrap()
                    .resolve_path(db, expr, path)
                    .unwrap();

                self.chunk.write_load_local_u8(local_idx as u8);
            }
            _ => {
                self.errs
                    .push(CompileError::UnexpectedExpr { expr: expr.clone() });
            }
        }
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

fn to_oper_f32(s: &str) -> Option<Op> {
    Some(match s {
        "+" => Op::AddF32,
        "-" => Op::SubF32,
        "*" => Op::MulF32,
        "/" => Op::DivF32,
        _ => return None,
    })
}
