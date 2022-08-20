//! Compiler (HIR → bytecode)

pub mod scope;

use la_arena::{ArenaMap, Idx};
use thiserror::Error;

use crate::{
    hir_def::{
        body::{
            expr::{self, Expr},
            pat::Pat,
            Body,
        },
        db::{vfs::*, *},
        ids::*,
        item_list::item,
    },
    syntax::{ast, ptr::AstPtr},
    vm::code::{Chunk, OpCode, TypedLiteral},
};

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("unexpected expr: {expr:?}")]
    UnexpectedExpr { expr: Expr },
    #[error("unexisting method call")]
    UnexistingMethodCall,
    #[error("can't resolve patern with name `{name}`")]
    CantResolvePattern { name: Name },
}

pub fn compile(db: &DB, krate: VfsFileId) -> (Chunk, Vec<CompileError>) {
    let mut compiler = Compiler::default();
    compiler.compile_crate(db, krate);
    (compiler.chunk, compiler.errs)
}

/// Compile-time call frame information
#[derive(Debug)]
struct CallFrame {
    proc_loc_id: HirItemLocId<item::DefProc>,
    locals: ArenaMap<Idx<Pat>, usize>,
    locals_capacity: usize,
}

impl CallFrame {
    pub fn new(db: &DB, proc_loc_id: HirItemLocId<item::DefProc>) -> Self {
        let body = db.proc_body(proc_loc_id);

        let bind_pats = body
            .pats
            .iter()
            .filter(|(_, p)| matches!(p, Pat::Bind { .. }))
            .map(|(idx, _)| idx);

        let mut locals = ArenaMap::default();
        let mut locals_capacity = 0;
        for (i, idx) in bind_pats.enumerate() {
            locals.insert(idx, i);
            locals_capacity += 1;
        }

        Self {
            proc_loc_id,
            locals,
            locals_capacity,
        }
    }

    /// Resolves pattern to local variable offset
    pub fn resolve_path(
        &self,
        db: &DB,
        path_expr_idx: Idx<expr::Expr>,
        path: &expr::Path,
    ) -> Option<usize> {
        let path_data = path.lookup(db);
        // FIXME: solve path
        let name = &path_data.segments[0];

        let binding_pat_id = {
            let expr_scopes = db.proc_expr_scope_map(self.proc_loc_id);
            let scope_id = expr_scopes.scope_for_expr(path_expr_idx)?;
            let entry = expr_scopes.resolve_name_in_scope_chain(scope_id, name)?;

            entry.pat
        };

        self.locals.get(binding_pat_id).map(|x| *x)
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
    pub fn compile_crate(&mut self, db: &DB, krate: VfsFileId) {
        let main_proc_id = self::find_procedure_in_crate(db, krate, &Name::from_str("main"));
        self.compile_proc(db, main_proc_id);
    }

    #[allow(unused)]
    fn compile_proc(&mut self, db: &DB, proc_loc_id: HirItemLocId<item::DefProc>) {
        let (body, body_source_map) = db.proc_body_with_source_map(proc_loc_id);

        // push frame
        {
            let call_frame = CallFrame::new(db, proc_loc_id);
            // FIXME:
            self.chunk
                .write_alloc_locals_u8(call_frame.locals_capacity as u8);
            self.call_frame = Some(call_frame);
        }

        // body
        {
            // Use AST to walk HIR expression in the occurence order
            let ast_proc = self::ast_proc(db, proc_loc_id);

            for ast_expr in ast_proc.block().exprs() {
                let ast_ptr = AstPtr::new(&ast_expr);
                let hir_expr_idx = body_source_map.expr_ast_hir[&ast_ptr];
                let hir_expr = &body.exprs[hir_expr_idx];
                self.compile_expr(db, &body, hir_expr_idx, hir_expr);
            }
        }

        // pop frame
        // FIXME: Handle frame stack pop and return value
        self.chunk.write_code(OpCode::OpReturn);
    }

    #[allow(unused)]
    fn compile_expr(&mut self, db: &DB, body: &Body, expr_idx: Idx<Expr>, expr: &Expr) {
        match expr {
            Expr::Call(call) => {
                let path = body.get_path(call.path);
                let path_data = path.lookup(db);

                // TODO: maybe support dot-separated path
                let name = path_data.segments[0].clone();

                match name.as_str() {
                    "+" | "-" | "*" | "/" => {
                        assert_eq!(
                            call.args.len(),
                            2,
                            "binary operator must take just two arguments"
                        );

                        let lhs_idx = call.args[0];
                        let lhs = &body.exprs[lhs_idx];
                        self.compile_expr(db, body, lhs_idx, &lhs);

                        let rhs_idx = call.args[1];
                        let rhs = &body.exprs[rhs_idx];
                        self.compile_expr(db, body, rhs_idx, &rhs);

                        // TODO: Don't assume `f32` type
                        let op = self::to_oper_f32(name.as_str()).unwrap();
                        self.chunk.write_code(op);
                    }
                    _ => {
                        todo!("{:?}", call);
                    }
                };
            }
            Expr::Let(let_) => {
                let rhs_idx = let_.rhs;
                let rhs = &body.exprs[rhs_idx];
                let rhs = self.compile_expr(db, body, rhs_idx, rhs);

                let local_idx = self.call_frame.as_ref().unwrap().locals[let_.pat];
                let local_idx = local_idx as u8;

                self.chunk.write_set_local_u8(local_idx);
            }
            Expr::Literal(lit) => match lit {
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
            Expr::Path(path) => {
                // let ast_id_map = db.ast_id_map(
                let local_idx = self
                    .call_frame
                    .as_ref()
                    .unwrap()
                    .resolve_path(db, expr_idx, path)
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

fn find_procedure_in_crate(
    db: &dyn Def,
    krate: vfs::VfsFileId,
    name: &Name,
) -> Id<HirItemLoc<item::DefProc>> {
    let crate_data = db.crate_data(krate);
    let crate_file_data = crate_data.root_file_data();
    let item_scope = &crate_file_data.item_scope;

    item_scope.lookup_proc(name).unwrap()
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

fn ast_proc(db: &DB, proc_loc_id: HirItemLocId<item::DefProc>) -> ast::DefProc {
    let proc_loc = proc_loc_id.lookup_loc(db);

    let items = db.file_item_list(proc_loc.file);
    let hir_proc = &items.procs[proc_loc.idx];

    let ast_id_map = db.ast_id_map(proc_loc.file);

    let ast_proc = {
        let parse = db.parse(proc_loc.file);
        let root_syntax = parse.doc.syntax();

        let ast_idx = hir_proc.ast_idx.clone();

        let ast_proc_ptr = ast_id_map.idx_to_ptr(ast_idx);

        ast_proc_ptr.to_node(&root_syntax)
    };

    ast_proc
}
