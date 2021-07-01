/*!
Bytecode, runtime representation of toylisp program
*/

pub mod compile;
pub mod data;
pub mod vm;

#[cfg(test)]
mod test {
    use crate::syntax::ast;

    fn arithmetics() {
        let src = "(/ (- 64.0 32.0) 2)";
        let (ast, errs) = ast::parse(src);

        if !errs.is_empty() {
            for e in errs {
                eprintln!("{}", e);
            }
            panic!();
        }
    }
}
