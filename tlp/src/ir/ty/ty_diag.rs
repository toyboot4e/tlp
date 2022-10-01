//! Diagonstics

use base::jar::InputFile;

use crate::{
    ir::{
        body::{expr::Expr, BodySpans},
        item,
        ty::Ty,
        IrDb, IrJar,
    },
    util::diag,
};

/// Type diagnostics for a procedure
#[salsa::accumulator(jar = IrJar)]
pub struct TypeDiagnostics(TypeDiagnostic);

crate::util::define_enum! {
    /// Type diagnostic for a procedure
    #[derive(Debug, Clone)]
    pub TypeDiagnostic = MissingParamType | MismatchedTypes | WrongArgTypes | CannotFindTypeInScope | CannotFindValueInScope;
}

impl diag::Diagnostic for TypeDiagnostic {
    fn code(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "E0000",
            TypeDiagnostic::MismatchedTypes(_) => "E0001",
            TypeDiagnostic::WrongArgTypes(_) => "E0002",
            TypeDiagnostic::CannotFindTypeInScope(_) => "E0020",
            TypeDiagnostic::CannotFindValueInScope(_) => "E0021",
        }
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "missing parameter type",
            TypeDiagnostic::MismatchedTypes(_) => "mismatched types",
            TypeDiagnostic::WrongArgTypes(_) => "wrong argument types",
            TypeDiagnostic::CannotFindTypeInScope(_) => "cannot find type in scope",
            TypeDiagnostic::CannotFindValueInScope(_) => "cannot find value in scope",
        }
    }

    fn reason(&self) -> &str {
        self.msg()
    }
}

impl TypeDiagnostic {
    pub fn render<'a>(
        &'a self,
        db: &'a dyn IrDb,
        input_file: InputFile,
        _proc: item::Proc,
        body_spans: &BodySpans,
    ) -> diag::Render<'a> {
        match self {
            TypeDiagnostic::MissingParamType(x) => {
                todo!()
            }
            TypeDiagnostic::MismatchedTypes(x) => {
                let span = body_spans[x.expr].as_ref().unwrap();
                diag::line(db, self, input_file, *span)
            }
            TypeDiagnostic::WrongArgTypes(_x) => todo!(),
            TypeDiagnostic::CannotFindTypeInScope(x) => {
                let span = body_spans[x.expr].as_ref().unwrap();
                diag::line(db, self, input_file, *span)
            }
            TypeDiagnostic::CannotFindValueInScope(x) => {
                let span = body_spans[x.expr].as_ref().unwrap();
                diag::line(db, self, input_file, *span)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct MissingParamType {
    pub param: item::Param,
}

#[derive(Debug, Clone)]
pub struct MismatchedTypes {
    pub expr: Expr,
    pub expected: Ty,
    pub actual: Ty,
    // why:
}

#[derive(Debug, Clone)]
pub struct WrongArgTypes {
    pub proc: Expr,
    pub wrong_args: Vec<(Expr, Ty)>,
}

#[derive(Debug, Clone)]
pub struct CannotFindTypeInScope {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct CannotFindValueInScope {
    pub expr: Expr,
}
