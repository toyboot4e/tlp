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
    println!("--------------------------------------------------------------------------------");
    println!("chunk:");
    for b in chunk.bytes() {
        println!("  {:b}", b);
    }
}

#[test]
fn simple_arithmetics() {
    // => 16.0
    let src = "(/ (- 64.0 32.0) 2.0)";

    let (doc, errs) = ast::parse(src).into_tuple();
    self::print_errors(&errs, src);

    let (chunk, errs) = compile::compile(doc);
    self::print_errors(&errs, src);

    log_chunk(&chunk);

    let mut vm = Vm::new(chunk);
    vm.run().unwrap();

    let val = vm.stack().last().unwrap();
    assert_eq!(*val, 16.0);
}
