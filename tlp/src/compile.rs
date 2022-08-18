//! Compiler (HIR → bytecode)

pub mod scope;

use thiserror::Error;

use crate::{
    hir_def::{
        body::Body,
        db::{ids::*, vfs::*, *},
        expr::{self, Expr},
        item::{self, Name},
    },
    vm::code::{Chunk, OpCode},
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
    fn compile_proc(&mut self, db: &DB, proc_id: Id<HirItemLoc<item::DefProc>>) {
        let body = db.proc_body(proc_id);
        let proc = self::get_proc(db, proc_id);

        // TODO: use source map pattern
        let ast_idx = proc.ast_idx.clone();
        let ast_id_map = db.ast_id_map(proc_id.lookup_loc(db).file);
        let ast = ast_id_map.idx_to_ast(ast_idx);

        // for expr in ast.block().exprs() {
        //     // TODO: convert AST expression into HIR expression and compile
        // }
    }

    #[allow(unused)]
    fn compile_expr(&mut self, db: &DB, body: &Body, expr: &Expr) {
        match expr {
            Expr::Call(call) => {
                let path = body.get_path(call.path);
                let path_data = path.lookup(db);

                // TODO: support path
                let name = path_data.segments[0].clone();

                match name.as_str() {
                    "+" | "-" | "*" | "/" => {
                        assert_eq!(call.args.len(), 2);

                        let lhs = &body.exprs[call.args[0]];
                        self.compile_expr(db, body, &lhs);

                        let rhs = &body.exprs[call.args[1]];
                        self.compile_expr(db, body, &rhs);

                        let op = self::to_oper(name.as_str()).unwrap();
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
                expr::Literal::Float(x) => {
                    let x = x.0 as f64;
                    let i = self.chunk.push_const(x);
                    self.chunk.push_ix_u8(i as u8);
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

fn to_oper(s: &str) -> Option<OpCode> {
    Some(match s {
        "+" => OpCode::OpAdd,
        "-" => OpCode::OpSub,
        "*" => OpCode::OpMul,
        "/" => OpCode::OpDiv,
        _ => return None,
    })
}

fn get_proc(db: &DB, proc_loc_id: Id<HirItemLoc<item::DefProc>>) -> item::DefProc {
    let proc_loc = proc_loc_id.lookup_loc(db);
    let items = db.file_item_list(proc_loc.file);
    let proc = &items.procs[proc_loc.idx];
    proc.clone()
}
