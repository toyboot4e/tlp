//! Compiler (IR → bytecode)

mod compile_proc;

use rustc_hash::FxHashMap;
use salsa::DebugWithDb;
use thiserror::Error;
use typed_index_collections::TiVec;

use base::jar::InputFile;

use crate::{
    ir::{
        body::{
            expr::{self, Expr},
            pat::{Pat, PatData},
        },
        item,
        resolve::ValueNs,
        ty, InputFileExt,
    },
    vm::{self, Vm},
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

        let mut compiler = compile_proc::CompileProc::new(&proc_ids, db, proc.clone());
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
