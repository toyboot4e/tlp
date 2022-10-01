//! Diagonstics

use base::jar::InputFile;

use crate::{
    ir::{
        body::{expr::Expr, BodySpans},
        item,
        ty::Ty,
        IrDb, IrJar,
    },
    util::diag::{self, Diagnostic, MsgSpan},
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
            TypeDiagnostic::MissingParamType(_x) => {
                todo!()
            }
            TypeDiagnostic::MismatchedTypes(x) => {
                let x = &x.mismatch;
                let main_span = body_spans.get(x.actual_expr).unwrap();
                let main_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::main_msg(db, self, input_file, main_msg)
            }
            TypeDiagnostic::WrongArgTypes(_x) => todo!(),
            TypeDiagnostic::IncompatibleOpArgTypes(x) => {
                let main_span = body_spans.get(x.op_expr).unwrap();

                let sub_msgs = vec![
                    MsgSpan {
                        span: body_spans.get(x.first_expr).unwrap(),
                        msg: "expected because of this argument".to_string(),
                    },
                    MsgSpan {
                        span: body_spans.get(x.mismatch.actual_expr).unwrap(),
                        msg: format!(
                            "expected `{}`, found `{}`",
                            x.mismatch.expected_ty.data(db).type_name(db),
                            x.mismatch.actual_ty.data(db).type_name(db),
                        ),
                    },
                ];

                diag::sub_msgs(db, self, input_file, main_span, sub_msgs)
            }
            TypeDiagnostic::CannotFindTypeInScope(x) => {
                let main_span = body_spans.get(x.expr).unwrap();
                let main_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::main_msg(db, self, input_file, main_msg)
            }
            TypeDiagnostic::CannotFindValueInScope(x) => {
                let main_span = *body_spans[x.expr].as_ref().unwrap();
                let main_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::main_msg(db, self, input_file, main_msg)
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
