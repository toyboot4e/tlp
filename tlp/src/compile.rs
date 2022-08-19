//! Compiler (HIR → bytecode)

pub mod scope;

use thiserror::Error;

use crate::{
    hir_def::{
        body::{
            expr::{self, Expr},
            Body,
        },
        db::{ids::*, vfs::*, *},
        item_list::item::{self, Name},
    },
    syntax::{ast, ptr::AstPtr},
    vm::code::{Chunk, OpCode, TypedLiteral},
};

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("Unexpected expr: {expr:?}")]
    UnexpectedExpr { expr: Expr },
    #[error("Unexisting method call")]
    UnexistingMethodCall,
}

pub fn compile(db: &DB, krate: VfsFileId) -> (Chunk, Vec<CompileError>) {
    let mut compiler = Compiler::default();
    compiler.compile_crate(db, krate);
    (compiler.chunk, compiler.errs)
}

#[derive(Debug, Default)]
struct Compiler {
    chunk: Chunk,
    errs: Vec<CompileError>,
}

impl Compiler {
    pub fn compile_crate(&mut self, db: &DB, krate: VfsFileId) {
        let main_proc_id = self::find_procedure_in_crate(db, krate, &Name::from_str("main"));
        self.compile_proc(db, main_proc_id);
    }

    #[allow(unused)]
    fn compile_proc(&mut self, db: &DB, proc_loc_id: Id<HirItemLoc<item::DefProc>>) {
        let (body, body_source_map) = db.proc_body_with_source_map(proc_loc_id);

        // Use AST to walk HIR expression in the occurence order
        let ast_proc = self::ast_proc(db, proc_loc_id);

        for ast_expr in ast_proc.block().exprs() {
            let ast_ptr = AstPtr::new(&ast_expr);
            let hir_expr_idx = body_source_map.expr_ast_hir[&ast_ptr];
            let hir_expr = &body.exprs[hir_expr_idx];
            self.compile_expr(db, &body, hir_expr);
        }
    }

    #[allow(unused)]
    fn compile_expr(&mut self, db: &DB, body: &Body, expr: &Expr) {
        match expr {
            Expr::Call(call) => {
                let path = body.get_path(call.path);
                let path_data = path.lookup(db);

                // TODO: maybe support dot-separated path
                let name = path_data.segments[0].clone();

                match name.as_str() {
                    "+" | "-" | "*" | "/" => {
                        assert_eq!(call.args.len(), 2);

                        let lhs = &body.exprs[call.args[0]];
                        self.compile_expr(db, body, &lhs);

                        let rhs = &body.exprs[call.args[1]];
                        self.compile_expr(db, body, &rhs);

                        // TODO: Don't assume `f32` type
                        let op = self::to_oper_f32(name.as_str()).unwrap();
                        self.chunk.push_code(op);
                    }
                    _ => {
                        todo!("{:?}", call);
                    }
                };
            }
            Expr::Let(_let_) => {
                // let pat = let_.pat().unwrap();
                // TODO: resolve
                todo!()
            }
            Expr::Literal(lit) => match lit {
                expr::Literal::F32(x) => {
                    let literal = TypedLiteral::F32(x.0);
                    let idx = self.chunk.store_literal(literal);
                    self.chunk.push_ix(idx);
                }
                _ => todo!(),
            },
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

fn ast_proc(db: &DB, proc_loc_id: Id<HirItemLoc<item::DefProc>>) -> ast::DefProc {
    let proc_loc = proc_loc_id.lookup_loc(db);

    let items = db.file_item_list(proc_loc.file);
    let hir_proc = &items.procs[proc_loc.idx];

    let item_source_map = db.item_source_map(proc_loc_id.lookup_loc(db).file);

    let ast_proc = {
        let parse = db.parse(proc_loc.file);
        let root_syntax = parse.doc.syntax();

        let ast_idx = hir_proc.ast_idx.clone();

        let ast_proc_ptr = item_source_map.idx_to_ptr(ast_idx);

        ast_proc_ptr.to_node(&root_syntax)
    };

    ast_proc
}
