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
        | IncompatibleOpArgTypes | IncorrectProcArgs
        | CannotFindTypeInScope | CannotFindValueInScope
        ;
}

impl diag::Diagnostic for TypeDiagnostic {
    fn code(&self) -> &str {
        match self {
            TypeDiagnostic::MissingParamType(_) => "E0000",
            TypeDiagnostic::MismatchedTypes(_) => "E0001",
            TypeDiagnostic::IncompatibleOpArgTypes(_) => "E0002",
            TypeDiagnostic::IncorrectProcArgs(_) => "E0003",
            TypeDiagnostic::CannotFindTypeInScope(_) => "E0020",
            TypeDiagnostic::CannotFindValueInScope(_) => "E0021",
        }
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> String {
        match self {
            TypeDiagnostic::MissingParamType(_) => "missing parameter type",
            TypeDiagnostic::MismatchedTypes(_) => "mismatched types",
            TypeDiagnostic::IncompatibleOpArgTypes(_) => "incompatible operator argument types",
            TypeDiagnostic::IncorrectProcArgs(x) => return x.primary_msg(),
            TypeDiagnostic::CannotFindTypeInScope(_) => "cannot find type in scope",
            TypeDiagnostic::CannotFindValueInScope(_) => "cannot find value in scope",
        }
        .to_string()
    }
}

pub fn eprint_many(
    db: &dyn IrDb,
    diags: &[TypeDiagnostic],
    input_file: InputFile,
    proc: item::Proc,
) {
    let body_spans = proc.body_spans(db);
    for diag in diags {
        eprintln!("{}", diag.render(db, input_file, proc, body_spans));
    }
}

impl TypeDiagnostic {
    pub fn render<'a>(
        &'a self,
        db: &'a dyn IrDb,
        input_file: InputFile,
        proc: item::Proc,
        body_spans: &BodySpans,
    ) -> diag::Render<'a> {
        // show procedure name
        let src_context = format!("(in proc `{}`)", proc.name(db).as_str(db.base()));

        match self {
            TypeDiagnostic::MissingParamType(_x) => {
                todo!()
            }
            TypeDiagnostic::MismatchedTypes(x) => {
                let x = &x.mismatch;
                let main_span = body_spans.get(x.actual_expr).unwrap();
                let primary_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::msg(db, self, input_file, src_context, primary_msg)
            }
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

                diag::multi_msgs(db, self, input_file, src_context, main_span, sub_msgs)
            }
            TypeDiagnostic::IncorrectProcArgs(x) => {
                let main_span = body_spans.get(x.proc_expr).unwrap();

                let sub_msgs = x
                    .type_mismatches
                    .iter()
                    .map(|type_mismatch| MsgSpan {
                        span: body_spans.get(type_mismatch.actual_expr).unwrap(),
                        msg: format!(
                            "expected `{}`, found `{}`",
                            type_mismatch.expected_ty.data(db).type_name(db),
                            type_mismatch.actual_ty.data(db).type_name(db),
                        ),
                    })
                    .collect::<Vec<_>>();

                // TODO: add node of the original procedure

                diag::multi_msgs(db, self, input_file, src_context, main_span, sub_msgs)
            }
            TypeDiagnostic::CannotFindTypeInScope(x) => {
                let main_span = body_spans.get(x.expr).unwrap();
                let primary_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::msg(db, self, input_file, src_context, primary_msg)
            }
            TypeDiagnostic::CannotFindValueInScope(x) => {
                let main_span = *body_spans[x.expr].as_ref().unwrap();
                let primary_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::msg(db, self, input_file, src_context, primary_msg)
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

#[derive(Debug, Clone)]
pub struct IncorrectProcArgs {
    pub proc_expr: Expr,
    /// Resolved procedure ID
    pub proc_id: item::Proc,
    pub type_mismatches: Vec<TypeMismatch>,
    pub arity_mismatch: Option<ArityMismatch>,
}

impl IncorrectProcArgs {
    // TODO: consider number
    pub fn primary_msg(&self) -> String {
        if let Some(arity) = &self.arity_mismatch {
            format!(
                "this procedure takes {} arguments but {} argumetns were given",
                arity.expected, arity.actual
            )
        } else {
            format!("arguments to this procedure are incorrect")
        }
    }
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

/// Arity mismatch information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArityMismatch {
    pub expected: usize,
    pub actual: usize,
}
