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

#[allow(unused)]
fn log_chunk(src: &str, chunk: &Chunk) {
    println!("Source:");
    println!("{}", src);
    println!("--------------------------------------------------------------------------------");

    let s = chunk.disassemble().unwrap();
    println!("{}", s);

    println!("--------------------------------------------------------------------------------");
}

fn test_expr<T: UnitVariant + PartialEq + std::fmt::Debug>(src: &str, expected: T) {
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

    log_chunk(&src, &chunk);

    let mut vm = Vm::new(chunk);
    vm.run().unwrap();

    let unit = vm.units().last().unwrap();
    assert_eq!(T::from_unit(*unit), expected);
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
fn boolean() {
    test_expr("true", true);
    test_expr("false", false);

    test_expr("(and true false)", false);
    test_expr("(or false true)", true);
    test_expr("(or true false)", true);

    test_expr("(let b false) (or b true)", true);
}
