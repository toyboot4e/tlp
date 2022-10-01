//! Diagonstics

use base::jar::InputFile;

use crate::{
    ir::{
        body::{expr::Expr, BodySpans},
        item,
        ty::Ty,
        IrDb, IrJar,
    },
    util::diag::{self, MsgSpan, Diagnostic},
};

/// Type diagnostics for a procedure
#[salsa::accumulator(jar = IrJar)]
pub struct TypeDiagnostics(TypeDiagnostic);

crate::util::define_enum! {
    /// Type diagnostic for a procedure
    #[derive(Debug, Clone)]
    pub TypeDiagnostic =
        | MissingParamType | MismatchedTypes
        | WrongArgTypes | IncompatibleOpArgTypes
        | CannotFindTypeInScope | CannotFindValueInScope
        ;
}

impl diag::Diagnostic for TypeDiagnostic {
    fn code(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "E0000",
            TypeDiagnostic::MismatchedTypes(_) => "E0001",
            TypeDiagnostic::WrongArgTypes(_) => "E0002",
            TypeDiagnostic::IncompatibleOpArgTypes(_) => "E0002",
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
            TypeDiagnostic::IncompatibleOpArgTypes(_) => "incompatible operator argument types",
            TypeDiagnostic::CannotFindTypeInScope(_) => "cannot find type in scope",
            TypeDiagnostic::CannotFindValueInScope(_) => "cannot find value in scope",
        }
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
                let x = &x.mismatch;
                let span = *body_spans[x.actual_expr].as_ref().unwrap();
                let msg_span = MsgSpan::new(span, self.msg());
                diag::line(db, self, input_file, msg_span)
            }
            TypeDiagnostic::WrongArgTypes(_x) => todo!(),
            TypeDiagnostic::IncompatibleOpArgTypes(x) => {
                let span = *body_spans[x.op_expr].as_ref().unwrap();
                let msg_span = MsgSpan::new(span, self.msg());
                // TODO: show sub diagnostics
                diag::line(db, self, input_file, msg_span)
            }
            TypeDiagnostic::CannotFindTypeInScope(x) => {
                let span = *body_spans[x.expr].as_ref().unwrap();
                let msg_span = MsgSpan::new(span, self.msg());
                diag::line(db, self, input_file, msg_span)
            }
            TypeDiagnostic::CannotFindValueInScope(x) => {
                let span = *body_spans[x.expr].as_ref().unwrap();
                let msg_span = MsgSpan::new(span, self.msg());
                diag::line(db, self, input_file, msg_span)
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
    pub mismatch: TypeMismatch,
}

#[derive(Debug, Clone)]
pub struct WrongArgTypes {
    pub proc: Expr,
    pub wrong_args: Vec<(Expr, Ty)>,
}

#[derive(Debug, Clone)]
pub struct IncompatibleOpArgTypes {
    pub op_expr: Expr,
    /// First typed expression that is used for detecting the type mistmatch
    pub first_expr: Expr,
    pub mismatch: TypeMismatch,
}

#[derive(Debug, Clone)]
pub struct CannotFindTypeInScope {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct CannotFindValueInScope {
    pub expr: Expr,
}

// --------------------------------------------------------------------------------
// Components
// --------------------------------------------------------------------------------

/// Type mismatch information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMismatch {
    pub expected_ty: Ty,
    pub actual_expr: Expr,
    pub actual_ty: Ty,
}

