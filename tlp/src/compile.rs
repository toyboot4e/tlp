//! Compiler (IR → bytecode)

mod compile_proc;
mod frame;

use rustc_hash::FxHashMap;
use thiserror::Error;
use typed_index_collections::TiVec;

use base::jar::InputFile;

use crate::{
    ir::{self, body::expr::Expr, item, ty, InputFileExt},
    syntax,
    vm::{self, Vm},
    Db,
};

pub enum CompileResult<'db> {
    LexError(&'db [syntax::cst::lex::LexError]),
    ParseError(&'db [syntax::ast::ParseError]),
    ItemError(Vec<ir::ir_diag::ItemDiagnostic>),
    BodyError(Vec<(ir::item::Proc, Vec<ty::ty_diag::TypeDiagnostic>)>),
    Success { vm: Vm, vm_errs: Vec<CompileError> },
}

impl<'db> CompileResult<'db> {
    pub fn print(&self, db: &Db, main_file: InputFile) {
        match self {
            Self::LexError(errors) => {
                for err in *errors {
                    println!("{}", err.render(db, main_file));
                }
            }
            Self::ParseError(errors) => {
                for err in *errors {
                    println!("{}", err.render(db, main_file));
                }
            }
            Self::ItemError(errors) => {
                for err in errors {
                    println!("{}", err.render(db, main_file));
                }
            }
            Self::BodyError(errors) => {
                for (proc, diags) in errors {
                    crate::ir::ty::ty_diag::eprint_many(db, &diags, main_file, *proc);
                }
            }
            Self::Success { .. } => {}
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum CompileError {
    #[error("unexpected expr: {expr:?}")]
    UnexpectedExpr { expr: Expr },
    #[error("unexisting method call")]
    UnexistingMethodCall,
    #[error("can't resolve patern with name `{name}`")]
    CantResolvePattern { name: String },
}

pub fn compile_file(db: &Db, main_file: InputFile) -> CompileResult {
    // let main_proc = self::find_procedure_by_name(db, main_file, "main");

    let parse = main_file.parse_items(db);

    let lex_errors = parse.lex_errors(db);
    if !lex_errors.is_empty() {
        return CompileResult::LexError(lex_errors);
    }

    let parse_errors = parse.errors(db);
    if !parse_errors.is_empty() {
        return CompileResult::ParseError(parse_errors);
    }

    let item_errors = main_file.item_syntax_diags(db);
    if !item_errors.is_empty() {
        return CompileResult::ItemError(item_errors);
    }

    self::compile_items(db, parse)
}

fn compile_items<'db>(db: &'db Db, parse: &ir::jar::ParsedFile) -> CompileResult<'db> {
    let mut body_errs = Vec::new();

    let mut vm_procs = TiVec::new();
    let mut vm_errs = Vec::new();

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

    for item in items {
        // TODO: use validated IR
        // FIXME: consider item declaration diagnostics
        let proc = match item {
            item::Item::Proc(proc) => proc,
        };

        // FIXME: consider procedure body diagnostics

        let errs = {
            let mut errs = Vec::new();

            // FIXME: `#[return_ref]`?
            let diags = proc.param_ty_diags(db);
            errs.extend(diags);

            let diags = proc.body_ty_diags(db);
            errs.extend(diags);

            errs
        };

        if !errs.is_empty() {
            body_errs.push((*proc, errs));
        }

        let mut compiler = compile_proc::CompileProc::new(&proc_ids, db, proc.clone());
        compiler.compile_proc();

        vm_procs.push(vm::VmProc {
            chunk: compiler.chunk,
            n_args: proc.body_data(db).param_pats.len(),
        });

        vm_errs.extend(compiler.errs);
    }

    if !body_errs.is_empty() {
        return CompileResult::BodyError(body_errs);
    }

    let vm = Vm::new(vm_procs);
    CompileResult::Success { vm, vm_errs }
}
