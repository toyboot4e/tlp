//! Runs given file

use std::{
    env, fmt, fs,
    io::{self, Write as _},
    path::PathBuf,
};

use base::jar::InputFile;

use tlp::{
    ir::{ir_diag::ItemDiagnostic, item, ty::ty_diag::TypeDiagnostic, InputFileExt, IrDb},
    util::diag::{self, Diagnostic},
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

    // TODO: don't count warnings
    let mut any_error = false;

    let out = io::stdout();
    let mut out = out.lock();

    let diags = main_file.item_syntax_diags(&db);
    any_error |= self::print_item_diagnostics(&mut out, &db, &diags, main_file).unwrap();

    for item in items {
        if let item::Item::Proc(proc) = item {
            let diags = proc.param_ty_diags(&db);
            any_error |=
                self::print_proc_diagonstics(&mut out, &db, &diags, main_file, *proc).unwrap();

            let diags = proc.body_ty_diags(&db);
            any_error |=
                self::print_proc_diagonstics(&mut out, &db, &diags, main_file, *proc).unwrap();
        }
    }

    any_error |= !errs.is_empty();
    self::print_errors(&mut out, &errs, &src, "compile error");
    out.flush().unwrap();

    if any_error {
        std::process::exit(1);
    }

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

fn print_item_diagnostics<'a>(
    out: &mut impl io::Write,
    db: &dyn IrDb,
    diags: &'a [ItemDiagnostic],
    input_file: InputFile,
) -> io::Result<bool> {
    if diags.is_empty() {
        return Ok(false);
    };

    let mut any_error = false;

    for diag in diags {
        any_error |= diag.severity() == diag::Severity::Error;
        writeln!(out, "{}", diag.render(db, input_file))?;
    }

    Ok(any_error)
}

/// Returns true on any error
fn print_proc_diagonstics<'a>(
    out: &mut impl io::Write,
    db: &dyn IrDb,
    diags: &'a [TypeDiagnostic],
    input_file: InputFile,
    proc: item::Proc,
) -> io::Result<bool> {
    if diags.is_empty() {
        return Ok(false);
    };

    let mut any_error = false;
    let body_spans = proc.body(db).spans(db);

    for diag in diags {
        any_error |= diag.severity() == diag::Severity::Error;
        writeln!(out, "{}", diag.render(db, input_file, proc, body_spans))?;
    }

    Ok(any_error)
}

fn print_errors(
    out: &mut impl io::Write,
    errs: &[impl fmt::Display],
    src: impl fmt::Display,
    header: impl ToString,
) {
    if errs.is_empty() {
        return;
    }

    writeln!(out, "{}", header.to_string()).unwrap();
    writeln!(out, "test code: {}", src).unwrap();
    writeln!(out, "errors:").unwrap();
    for e in errs {
        writeln!(out, "- {} ", e).unwrap();
    }
}
