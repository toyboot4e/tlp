//! Compiler (HIR → bytecode)

pub mod scope;

use rustc_hash::FxHashMap;
use thiserror::Error;

use base::jar::{InputFile, Word};

use crate::{
    ir::{
        body::{
            expr::{self, Expr, ExprData},
            pat::{Pat, PatData},
            Body, BodyData,
        },
        item,
    },
    vm::code::{Chunk, OpCode, TypedLiteral},
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
            for &expr in body_data.root_block() {
                let expr_data = &body_data.tables[expr];
                self.compile_expr(db, &body_data, expr, expr_data);
            }
        }

        // pop frame
        // FIXME: Handle frame stack pop and return value
        self.chunk.write_code(OpCode::OpReturn);
    }

    #[allow(unused)]
    fn compile_expr(&mut self, db: &Db, body_data: &BodyData, expr: Expr, expr_data: &ExprData) {
        match expr_data {
            ExprData::Call(call) => {
                let path = body_data.tables[call.path].clone().into_path();

                // TODO: maybe support dot-separated path
                let name = path.segments[0].clone();

                match name.as_str(db) {
                    "+" | "-" | "*" | "/" => {
                        assert_eq!(
                            call.args.len(),
                            2,
                            "binary operator must take just two arguments"
                        );

                        let lhs_idx = call.args[0];
                        let lhs = &body_data.tables[lhs_idx];
                        self.compile_expr(db, body_data, lhs_idx, &lhs);

                        let rhs_idx = call.args[1];
                        let rhs = &body_data.tables[rhs_idx];
                        self.compile_expr(db, body_data, rhs_idx, &rhs);

                        // FIXME: Don't assume `f32` type
                        let op = self::to_oper_f32(name.as_str(db)).unwrap();
                        self.chunk.write_code(op);
                    }
                    _ => {
                        todo!("{:?}", call);
                    }
                };
            }
            ExprData::Let(let_) => {
                let rhs_idx = let_.rhs;
                let rhs = &body_data.tables[rhs_idx];
                let rhs = self.compile_expr(db, body_data, rhs_idx, rhs);

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

fn to_oper_f32(s: &str) -> Option<OpCode> {
    Some(match s {
        "+" => OpCode::OpAddF32,
        "-" => OpCode::OpSubF32,
        "*" => OpCode::OpMulF32,
        "/" => OpCode::OpDivF32,
        _ => return None,
    })
}
