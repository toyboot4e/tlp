//! Diagonstics

use base::{jar::InputFile, span::Span};

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
        | MismatchedTypes
        | IncompatibleOpArgTypes | IncorrectProcArgs
        | CannotFindTypeInScope | CannotFindValueInScope
        ;
}

impl diag::Diagnostic for TypeDiagnostic {
    fn code(&self) -> Option<&'static str> {
        Some(match self {
            TypeDiagnostic::MismatchedTypes(_) => "E0200",
            TypeDiagnostic::IncompatibleOpArgTypes(_) => "E0201",
            TypeDiagnostic::IncorrectProcArgs(_) => "E0202",
            TypeDiagnostic::CannotFindTypeInScope(_) => "E0210",
            TypeDiagnostic::CannotFindValueInScope(_) => "E0211",
        })
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    fn msg(&self) -> String {
        match self {
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
        let src_context = format!("(`{}`)", proc.name(db).as_str(db.base()));

        match self {
            TypeDiagnostic::MismatchedTypes(x) => {
                let x = &x.mismatch;
                let main_span = body_spans.get(x.actual_expr).unwrap();
                let primary_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::render_single_msg(db.base(), self, input_file, src_context, primary_msg)
            }
            TypeDiagnostic::IncompatibleOpArgTypes(x) => {
                let main_span = body_spans.get(x.op_expr).unwrap();

                let sub_msgs = vec![
                    MsgSpan::new(
                        body_spans.get(x.first_expr).unwrap(),
                        "expected because of this argument".to_string(),
                    ),
                    MsgSpan::new(
                        body_spans.get(x.mismatch.actual_expr).unwrap(),
                        format!(
                            "expected `{}`, found `{}`",
                            x.mismatch.expected_ty.data(db).type_name(db),
                            x.mismatch.actual_ty.data(db).type_name(db),
                        ),
                    ),
                ];

                diag::render_multi_msg(db.base(), self, input_file, src_context, main_span, sub_msgs)
            }
            TypeDiagnostic::IncorrectProcArgs(x) => {
                let main_span = body_spans.get(x.proc_expr).unwrap();
                let sub_msgs = TypeMismatch::msg_spans(
                    x.type_mismatches.iter().map(|(_, x)| x),
                    db,
                    body_spans,
                );

                let mut render =
                    diag::render_multi_msg(db.base(), self, input_file, src_context, main_span, sub_msgs);

                let params = x.proc_id.params(db);
                let param_tys = &x.proc_id.ty_data_as_proc(db).param_tys;

                let param_spans = x
                    .type_mismatches
                    .iter()
                    .map(|(i, mismatch)| {
                        debug_assert_eq!(param_tys[*i], mismatch.expected_ty);
                        params[*i].span
                    })
                    .collect::<Vec<_>>();

                render.sub_renders =
                    vec![ProcedureDefinedHere::new(x.proc_id, param_spans).render(db)];
                // TODO: add node of the original procedure

                render
            }
            TypeDiagnostic::CannotFindTypeInScope(x) => {
                let main_span = body_spans.get(x.expr).unwrap();
                let primary_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::render_single_msg(db.base(), self, input_file, src_context, primary_msg)
            }
            TypeDiagnostic::CannotFindValueInScope(x) => {
                let main_span = *body_spans[x.expr].as_ref().unwrap();
                let primary_msg = MsgSpan::new(main_span, self.msg().to_string());
                diag::render_single_msg(db.base(), self, input_file, src_context, primary_msg)
            }
        }
    }
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
    /// (nth_arg, mismatch)
    pub type_mismatches: Vec<(usize, TypeMismatch)>,
    pub arity_mismatch: Option<ArityMismatch>,
}

impl IncorrectProcArgs {
    // TODO: consider number
    pub fn primary_msg(&self) -> String {
        if let Some(arity) = &self.arity_mismatch {
            fn arity_to_arg(arity: usize) -> &'static str {
                match arity {
                    0 | 1 => "argument",
                    _ => "arguments",
                }
            }

            format!(
                "this procedure takes {} {} but {} {} were given",
                arity.expected,
                arity_to_arg(arity.expected),
                arity.actual,
                arity_to_arg(arity.actual),
            )
        } else {
            format!("incorrect procedure arguments")
        }
    }
}

// --------------------------------------------------------------------------------
// Sub diagnostics
// --------------------------------------------------------------------------------

/// Shown on type mismatch
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcedureDefinedHere {
    pub proc: item::Proc,
    /// Parameters to draw underlines
    pub param_spans: Vec<Span>,
}

impl Diagnostic for ProcedureDefinedHere {
    fn severity(&self) -> diag::Severity {
        diag::Severity::Note
    }

    fn msg(&self) -> String {
        "function defined here".to_string()
    }
}

impl ProcedureDefinedHere {
    pub fn new(proc: item::Proc, param_spans: Vec<Span>) -> Self {
        Self {
            proc,
            param_spans,
        }
    }

    pub fn render<'a>(&self, db: &'a dyn IrDb) -> diag::Render<'a> {
        let primary_span = self.proc.name(db).span(db.base()).span();

        let sub_msgs = self.param_spans
            .iter()
            .map(|span| MsgSpan::span_only(*span))
            .collect::<Vec<_>>();

        diag::render_multi_msg(
            db.base(),
            self,
            self.proc.input_file(db),
            // no context
            "".to_string(),
            primary_span,
            sub_msgs,
        )
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

impl TypeMismatch {
    pub fn msg_spans<'a>(
        xs: impl Iterator<Item = &'a TypeMismatch>,
        db: &dyn IrDb,
        body_spans: &BodySpans,
    ) -> Vec<MsgSpan> {
        xs.map(|type_mismatch| {
            MsgSpan::new(
                body_spans.get(type_mismatch.actual_expr).unwrap(),
                format!(
                    "expected `{}`, found `{}`",
                    type_mismatch.expected_ty.data(db).type_name(db),
                    type_mismatch.actual_ty.data(db).type_name(db),
                ),
            )
        })
        .collect::<Vec<_>>()
    }
}

/// Arity mismatch information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArityMismatch {
    pub expected: usize,
    pub actual: usize,
}
