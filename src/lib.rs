/*!
ToyLisp is a dialect of Lisp for my game dev (?)
*/

pub mod compile;
pub mod lex;
pub mod span;
pub mod vm;

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple() -> anyhow::Result<()> {
        let src = "(/ (- 64.0 32.0) 2)";
        //         01 34 5678 01234 67
        //         0          1

        let mut vm = {
            let ast = lex::sx::from_str(src)?;
            let chunk = compile::from_file(&ast)?;
            crate::vm::Vm::new(chunk)
        };

        vm.run()?;
        assert_eq!(Some(&16.0), vm.stack().last());

        Ok(())
    }
}
