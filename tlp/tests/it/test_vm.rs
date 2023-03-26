//! VM tests.

/// Hard-coded VM run tests.
mod run_tests;

// data-driven tests

use std::fmt::Write;

use tlp::{compile::CompileResult, Db};

use crate::util::{self, Test, TestError};

/// Runs tests defined in the test case files
#[test]
fn bytecode() {
    let src = include_str!("test_cases/bytecode_test_cases.txt");
    util::run_tests(src, runner)
}

fn runner(test: Test) -> Result<(), TestError> {
    let mut db = Db::default();

    let main_file = db.new_input_file("main.tlp", test.code.to_string());
    let (vm, errs) = {
        let result = tlp::compile::compile_file(&db, main_file);
        match result {
            CompileResult::Success { vm, vm_errs } => (vm, vm_errs),
            _ => {
                let mut s = String::new();
                result.write(&mut s, &db, main_file).unwrap();

                return Err(TestError {
                    test: test.clone(),
                    output: format!("unable to compile {}:\n{}", test.title, s),
                });
            }
        }
    };

    // TODO: print VM errors
    if !errs.is_empty() {
        let mut s = String::new();
        for err in errs {
            writeln!(s, "- {:?}", err).unwrap();
        }

        return Err(TestError {
            test: test.clone(),
            output: format!("compile error {}n{}", test.title, s),
        });
    }

    let bytecode = vm
        .procs()
        .iter()
        .map(|proc| proc.chunk.disassemble_with_name(&proc.name).unwrap())
        .collect::<Vec<_>>()
        .join("\n");

    test.result(&bytecode)
}
