//! VM tests

use std::fmt::{self, Write};

use tlp::{
    compile,
    syntax::ast,
    vm::{code::Chunk, Vm},
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
    for b in chunk.bytes() {
        println!("  {:b}", b);
    }
    println!("--------------------------------------------------------------------------------");
}

fn run_test(src: &str, expected: f64) {
    let (doc, errs) = ast::parse(src).into_tuple();
    self::print_errors(&errs, src);

    let (chunk, errs) = compile::compile(doc);
    self::print_errors(&errs, src);

    // log_chunk(&chunk);

    let mut vm = Vm::new(chunk);
    vm.run().unwrap();

    let val = vm.stack().last().unwrap();
    assert_eq!(*val, expected);
}

#[test]
fn simple_arithmetics() {
    run_test("(/ (- 64.0 32.0) 2.0)", 16.0);
    run_test("(+ (* 3.0 4.0) 2.0)", 14.0);
    run_test("(let a 10.0) (+ a 2.0)", 12.0);
}
