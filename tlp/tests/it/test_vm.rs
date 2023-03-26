//! VM tests.

/// Hard-coded VM run tests.
mod run_tests;

// data-driven tests

use tlp::{compile::CompileResult, syntax, Db};

use crate::util::{self, Test, TestError};

/// Runs tests defined in the test case files
#[test]
fn bytecode() {
    let src = include_str!("test_cases/bytecode_test_cases.txt");
    util::run_tests(src, runner)
}

// TODO: implement
fn runner(test: Test) -> Result<(), TestError> {
    // let mut db = Db::default();

    // let main_file = db.new_input_file("main.tlp", test.code.to_string());
    // let (mut vm, errs) = match tlp::compile::compile_file(&db, main_file) {
    //     CompileResult::Success { vm, vm_errs } => (vm, vm_errs),
    //     _ => {
    //         panic!("unable to compile {}", test.title);
    //     }
    // };

    Ok(())
}
