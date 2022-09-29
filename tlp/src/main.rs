//! Runs given file

use std::{
    env,
    fmt::{self, Write},
    fs,
    path::PathBuf,
};

use salsa::DebugWithDb;

use tlp::{
    ir::{item, InputFileExt},
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
    let mut s = String::new();

    for item in items {
        if let item::Item::Proc(proc) = item {
            let dcx = proc.with_db(&db);

            // let body_spans = proc.body(&db).spans(&db);

            let diags = proc.param_ty_diags(&db);
            for diag in diags {
                write!(s, "{:?}", diag.debug(&dcx)).unwrap();
            }

            let diags = proc.body_ty_diags(&db);
            for diag in diags {
                write!(s, "{:?}", diag.debug(&dcx)).unwrap();
            }
        }
    }

    println!("{}", s);

    self::print_errors(&errs, &src, "compile error");

    // TODO: search main proc
    let proc = tlp::vm::VmProcId(0);

    match vm.run_proc(proc) {
        Ok(unit) => println!("=> {:?}", unit),
        Err(e) => {
            // TODO: use `log_vm` in `test_vm.rs`
            panic!("error: {}", e);
        }
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
