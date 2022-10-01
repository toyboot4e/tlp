//! Runs given file

use std::{
    env,
    fmt::{self, Write},
    fs,
    path::PathBuf,
};

use base::jar::InputFile;

use tlp::{
    ir::{item, ty::ty_diag::TypeDiagnostic, InputFileExt, IrDb},
    util::diag,
    vm::UnitVariant,
    Db,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = PathBuf::from(&args[1]);

    let src = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("can't open file {:?}: {}", path, err));

    let mut db = Db::default();

    let main_file = db.new_input_file("main.tlp", src.to_string());
    let (mut vm, errs) = tlp::compile::compile_file(&db, main_file);

    let items = main_file.items(&db);

    for item in items {
        if let item::Item::Proc(proc) = item {
            let diags = proc.param_ty_diags(&db);
            self::print_diagnostics(&db, diags.iter(), *proc, main_file);

            let diags = proc.body_ty_diags(&db);
            self::print_diagnostics(&db, diags.iter(), *proc, main_file);
        }
    }

    self::print_errors(&errs, &src, "compile error");

    // TODO: search main proc
    let proc = tlp::vm::VmProcId(0);

    match vm.run_proc(proc) {
        Ok(unit) => {
            let res = u32::from_unit(unit);
            println!("=> {:?}", res);
        }
        Err(e) => {
            // TODO: use `log_vm` in `test_vm.rs`
            panic!("error: {}", e);
        }
    }
}

fn print_diagnostics<'a>(
    db: &dyn IrDb,
    diags: impl Iterator<Item = &'a TypeDiagnostic>,
    proc: item::Proc,
    input_file: InputFile,
) {
    let body_spans = proc.body(db).spans(db);

    for diag in diags {
        let span = match diag {
            TypeDiagnostic::MissingParamType(x) => {
                todo!()
            }
            TypeDiagnostic::TypeMismatch(x) => &body_spans[x.expr],
            TypeDiagnostic::CantResolve(x) => &body_spans[x.expr],
        }
        .as_ref()
        .unwrap();

        diag::line(db, diag, input_file, *span).print();
    }
}

fn print_errors(errs: &[impl fmt::Display], src: impl fmt::Display, header: impl ToString) {
    if errs.is_empty() {
        return;
    }

    let mut s = String::new();

    writeln!(s, "{}", header.to_string()).unwrap();
    writeln!(s, "test code: {}", src).unwrap();
    writeln!(s, "errors:").unwrap();
    for e in errs {
        writeln!(s, "- {} ", e).unwrap();
    }

    panic!("{}", s);
}
