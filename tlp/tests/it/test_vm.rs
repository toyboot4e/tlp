//! VM tests

use std::fmt::{self, Write};

use tlp::{
    compile,
    syntax::ast,
    vm::{
        code::{Chunk, OpCode, OpCodeOperands},
        UnitVariant, Vm,
    },
    Db,
};

fn print_errors(errs: &[impl fmt::Display], src: impl fmt::Display) {
    if errs.is_empty() {
        return;
    }

    let mut s = String::new();

    writeln!(s, "source: {}", src).unwrap();
    for e in errs {
        writeln!(s, "- {} ", e).unwrap();
    }

    panic!("{}", s);
}

#[allow(unused)]
fn log_chunk(chunk: &Chunk) {
    println!("");
    println!("--------------------------------------------------------------------------------");

    let s = chunk.disassemble().unwrap();
    println!("{}", s);

    println!("--------------------------------------------------------------------------------");
}

fn test_expr(src: &str, expected: impl UnitVariant) {
    let (_doc, errs) = ast::parse(src).into_tuple();
    self::print_errors(&errs, src);

    let chunk = {
        let mut db = Db::default();

        let src = format!("(proc main () {} )", src);
        let file = db.new_input_file("main.tlp", src.clone());

        let (chunk, errs) = compile::compile(&db, file);
        self::print_errors(&errs, &src);
        chunk
    };

    log_chunk(&chunk);

    let mut vm = Vm::new(chunk);
    vm.run().unwrap();

    let unit = vm.units().last().unwrap();
    assert_eq!(*unit, expected.into_unit());
}

#[test]
fn simple_arithmetics() {
    test_expr("(/ (- 64.0 32.0) 2.0)", 16.0);
    test_expr("(+ (* 3.0 4.0) 2.0)", 14.0);
}

#[test]
fn let_statement() {
    test_expr("(let a 10.0) (+ a 2.0)", 12.0);
    test_expr("(let a 10) (+ a 2)", 12);
}
