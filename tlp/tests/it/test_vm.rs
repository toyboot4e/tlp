//! VM tests

use std::fmt::{self, Write};

use tlp::{
    compile,
    syntax::ast,
    vm::{self, Unit, UnitVariant, Vm},
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

fn log_vm(src: &str, vm: &Vm) -> Result<String, fmt::Error> {
    let mut s = String::new();

    writeln!(
        s,
        "--------------------------------------------------------------------------------"
    )?;

    writeln!(s, "Source: {}", src)?;

    writeln!(
        s,
        "--------------------------------------------------------------------------------"
    )?;

    for (i, chunk) in vm.proc_chunks().iter().enumerate() {
        writeln!(s, "proc {}:", i)?;
        writeln!(s, "{}", chunk.disassemble().unwrap())?;
    }

    writeln!(
        s,
        "--------------------------------------------------------------------------------"
    )?;

    Ok(s)
}

fn run<T: UnitVariant + PartialEq + std::fmt::Debug>(src: &str) -> (Vm, Unit) {
    {
        let (_doc, errs) = ast::parse(src).into_tuple();
        self::print_errors(&errs, src, "parse error");
    }

    let (mut vm, errs) = {
        let mut db = Db::default();
        let file = db.new_input_file("main.tlp", src.to_string());
        compile::compile_file(&db, file)
    };

    self::print_errors(&errs, src, "compile error");

    // TODO: print procedure name
    println!("{}", log_vm(&src, &vm).unwrap());

    // TODO: search functions
    let proc = vm::VmProcId(0);

    match vm.run_proc(proc) {
        Ok(unit) => (vm, unit),
        Err(e) => {
            panic!("{}\n{}", e, log_vm(&src, &vm).unwrap());
        }
    }
}

fn run_expr<T: UnitVariant + PartialEq + std::fmt::Debug>(src: &str) -> (Vm, Unit) {
    let src = format!("(proc main () {})", src);
    self::run::<T>(&src)
}

fn test_impl<T: UnitVariant + PartialEq + std::fmt::Debug + Clone>(
    src: &str,
    expected: T,
    vm: Vm,
    unit: Unit,
) {
    // check the last and the only value
    assert_eq!(
        T::from_unit(unit),
        expected,
        "{}",
        log_vm(&src, &vm).unwrap()
    );
}

fn test_expr<T: UnitVariant + PartialEq + std::fmt::Debug + Clone>(src: &str, expected: T) {
    let (vm, unit) = self::run_expr::<T>(src);
    self::test_impl(src, expected, vm, unit)
}

fn test_file<T: UnitVariant + PartialEq + std::fmt::Debug + Clone>(src: &str, expected: T) {
    let (vm, unit) = self::run::<T>(src);
    self::test_impl(src, expected, vm, unit)
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
fn comparison() {
    // bool
    test_expr("(= true true)", true);
    test_expr("(= true false)", false);
    test_expr("(= false true)", false);
    test_expr("(= false false)", true);

    test_expr("(!= true true)", !true);
    test_expr("(!= true false)", !false);
    test_expr("(!= false true)", !false);
    test_expr("(!= false false)", !true);

    // i32
    test_expr("(= 0 2)", false);
    test_expr("(= 2 2)", true);

    test_expr("(!= 0 2)", !false);
    test_expr("(!= 2 2)", !true);

    test_expr("(< 2 4)", true);
    test_expr("(< 3 3)", false);
    test_expr("(< 4 2)", false);

    test_expr("(<= 2 4)", true);
    test_expr("(<= 3 3)", true);
    test_expr("(<= 4 2)", false);

    test_expr("(> 2 4)", false);
    test_expr("(> 3 3)", false);
    test_expr("(> 4 2)", true);

    test_expr("(>= 2 4)", false);
    test_expr("(>= 3 3)", true);
    test_expr("(>= 4 2)", true);

    // f32
    test_expr("(= 0.0 2.2)", false);
    test_expr("(= 2.2 2.2)", true);

    test_expr("(!= 0.0 2.2)", !false);
    test_expr("(!= 2.2 2.2)", !true);

    test_expr("(< 2.2 4.4)", true);
    test_expr("(< 3.3 3.3)", false);
    test_expr("(< 4.4 2.2)", false);

    test_expr("(<= 2.2 4.4)", true);
    test_expr("(<= 3.3 3.3)", true);
    test_expr("(<= 4.4 2.2)", false);

    test_expr("(> 2.2 4.4)", false);
    test_expr("(> 3.3 3.3)", false);
    test_expr("(> 4.4 2.2)", true);

    test_expr("(>= 2.2 4.4)", false);
    test_expr("(>= 3.3 3.3)", true);
    test_expr("(>= 4.4 2.2)", true);
}

#[test]
fn control_flow() {
    test_expr("(let a 0) (when true (set a 10)) a", 10);
    test_expr("(unless false true) 15", 15);

    // cond expression
    test_expr("(cond (false 10) (true 15))", 15);
    test_expr("(cond (true 10) (false 15))", 10);

    // cond statement
    test_expr("(cond (false 10) (false 15))", 0);

    // while
    test_expr(
        "
(let a 0)
(while (< a 3)
    (set a (+ a 1)))
a",
        3,
    );

    // TODO: +=, -=, inc, dec, inc-mut?, dec-mut?
}

#[test]
fn shadowing() {
    test_expr(
        "(let a 5) (let out a) (let a 10) (set out (+ out a)) out",
        15,
    )
}

#[test]
fn user_function_call() {
    // function with no argument
    test_file(
        "
(proc main ()
    (+ 5 (f)))
(proc f ()
    15)
",
        20,
    );

    // function with arguments
    test_file(
        "
(proc main ()
    (f 10))
(proc f (x)
    (+ x 5))
    ",
        15,
    );

    // recursive function call
    fn fib(x: usize) -> usize {
        match x {
            0 => 0,
            1 => 1,
            _ => fib(x - 1) + fib(x - 2),
        }
    }

    test_file(
        "
(proc main ()
    (fib 10))

(proc fib (x)
    (cond ((= x 0) 0)
          ((= x 1) 1)
          (true (+ (fib (- x 1)) (fib (- x 2))))))
",
        fib(10) as u32,
    );
}

// TODO: +=, -=, inc, dec, inc-mut?, dec-mut?
