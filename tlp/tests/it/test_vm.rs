//! VM tests

use std::fmt::{self, Write};

use tlp::{
    compile,
    syntax::ast,
    vm::{code::Chunk, UnitVariant, Vm},
    Db,
};

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

fn log_chunk(src: &str, chunk: &Chunk) -> Result<String, fmt::Error> {
    let mut s = String::new();

    writeln!(s, "Source: {}", src)?;

    writeln!(
        s,
        "--------------------------------------------------------------------------------"
    )?;

    writeln!(s, "{}", chunk.disassemble().unwrap())?;

    writeln!(
        s,
        "--------------------------------------------------------------------------------"
    )?;

    Ok(s)
}

fn run<T: UnitVariant + PartialEq + std::fmt::Debug>(src: &str, expected: T) -> (Chunk, Vm) {
    let (_doc, errs) = ast::parse(src).into_tuple();
    self::print_errors(&errs, src, "parse error");

    let src = format!("(proc main () {})", src);
    let chunk = {
        let mut db = Db::default();

        let file = db.new_input_file("main.tlp", src.clone());

        let (chunk, errs) = compile::compile(&db, file);
        self::print_errors(&errs, &src, "compile error");
        chunk
    };

    // println!("{}", log_chunk(&src, &chunk).unwrap());

    let mut vm = Vm::new(chunk.clone());
    if let Err(e) = vm.run() {
        panic!("{}\n{}", e, log_chunk(&src, &chunk).unwrap());
    }

    (chunk, vm)
}

fn test_expr<T: UnitVariant + PartialEq + std::fmt::Debug + Clone>(src: &str, expected: T) {
    let (chunk, vm) = self::run(src, expected.clone());

    let unit = match vm.units().last() {
        Some(x) => x,
        None => panic!(
            "Nothing on stack after run.\n{}",
            log_chunk(&src, &chunk).unwrap()
        ),
    };

    // balance test
    assert_eq!(
        vm.units().len(),
        vm.stack().tmp_offset() + 1,
        "stack is not balanced: {:?}\n{}",
        vm.units(),
        log_chunk(&src, &chunk).unwrap()
    );

    // check the last and the only value
    assert_eq!(
        T::from_unit(*unit),
        expected,
        "{}",
        log_chunk(&src, &chunk).unwrap()
    );
}

#[test]
fn simple_arithmetics() {
    // f32
    test_expr("(/ (- 13.2 4.2) 2.0)", 4.5);
    test_expr("(+ (* 3.0 4.0) 2.5)", 14.5);

    // i32
    test_expr("(/ (- 64 32) 2)", 16);
    test_expr("(+ (* 3 4) 2)", 14);
}

#[test]
fn let_statement() {
    test_expr("(let a 10.5) (+ a 2.5)", 13.0);
    test_expr("(let a 10) (+ a 2)", 12);
}

#[test]
fn bool_literal() {
    test_expr("true", true);
    test_expr("false", false);
}

#[test]
fn bool_and_or() {
    test_expr("(and false false)", false);
    test_expr("(and true false)", false);
    test_expr("(and false true)", false);
    test_expr("(and true true)", true);

    test_expr("(or false false)", false);
    test_expr("(or true false)", true);
    test_expr("(or false true)", true);
    test_expr("(or true true)", true);

    test_expr("(let b false) (or b true)", true);
    test_expr("(let b true) (and b true)", true);
}

#[test]
fn stack_balance() {
    // REMARK: Statement returns `<none>`, which internally is `0`
    test_expr("(let a 0)", 0);

    test_expr("(when false true)", 0);
    test_expr("(when true true)", 0);

    test_expr("(unless false true)", 0);
    test_expr("(unless true true)", 0);
}

#[test]
fn control_flow() {
    test_expr("(let a 0) (when true (set a 10)) a", 10);
    test_expr("(unless false true) 15", 15);

    test_expr("(cond (false 10) (true 15))", 15);
    test_expr("(cond (true 10) (false 15))", 10);
}
