//! IR diagnostings: items and bodies

use base::{jar::InputFile, span::Span};

use crate::{
    ir::{item, IrDb, IrJar},
    util::diag::{self, Diagnostic},
};

/// Item lowering diagnostics
#[salsa::accumulator(jar = IrJar)]
pub struct ItemDiagnostics(ItemDiagnostic);

crate::util::define_enum! {
    /// IR lowering diagnostics
    #[derive(Debug, Clone)]
    pub ItemDiagnostic =
        | MissingProcName | ProcDiagnostic
        ;
}

impl diag::Diagnostic for ItemDiagnostic {
    fn code(&self) -> Option<&'static str> {
        Some(match self {
            ItemDiagnostic::MissingProcName(_) => "E0100",
            ItemDiagnostic::ProcDiagnostic(_) => "E0101",
        })
    }

    fn severity(&self) -> diag::Severity {
        diag::Severity::Error
    }

    // FIXME: duplicate messages
    fn msg(&self) -> String {
        match self {
            ItemDiagnostic::MissingProcName(_) => "missing procedure name",
            ItemDiagnostic::ProcDiagnostic(x) => match (!x.params.is_empty(), x.missing_ret_ty) {
                // (param_ty error, ret_ty error)
                (true, true) => "missing type annotations",
                (true, false) => "missing parameter type",
                (false, true) => "missing return type",
                (false, false) => unreachable!(),
            },
        }
        .to_string()
    }
}

/// If the procedure is missing their name, we only report that without checking parameters and
/// return types
#[derive(Debug, Clone)]
pub struct MissingProcName {
    // no `Proc` since we don't deal the syntax as a procedure
    pub span: Span,
}

/// If the procedure has name, we report detailed diagnostics
#[derive(Debug, Clone)]
pub struct ProcDiagnostic {
    pub proc: item::Proc,
    pub params: Vec<ProcParamDiagnostic>,
    pub missing_ret_ty: bool,
}

impl ProcDiagnostic {
    pub fn new(proc: item::Proc) -> Self {
        Self {
            proc,
            params: Default::default(),
            missing_ret_ty: Default::default(),
        }
    }

    pub fn any_error(&self) -> bool {
        !self.params.is_empty() || self.missing_ret_ty
    }
}

#[derive(Debug, Clone)]
pub struct ProcParamDiagnostic {
    /// Nth parameter
    pub index: usize,
    // pub name_missing: bool,
    pub type_missing: bool,
}

impl ItemDiagnostic {
    pub fn render<'a>(&'a self, db: &'a dyn IrDb, input_file: InputFile) -> diag::Render<'a> {
        match self {
            ItemDiagnostic::MissingProcName(x) => {
                let src_context = "".to_string();
                let msg_span = diag::MsgSpan::new(x.span, self.msg());
                diag::render_single_msg(db.base(), self, input_file, src_context, msg_span)
            }
            ItemDiagnostic::ProcDiagnostic(x) => {
                assert!(x.any_error());

                let proc = x.proc;
                let src_context = format!("(`{}`)", proc.name(db).as_str(db.base()));
                let primary_span = proc.name(db).span(db.base()).span();

                let params = proc.params(db);
                let mut secondary_msgs = Vec::new();

                for diag in &x.params {
                    let param = &params[diag.index];
                    let msg_span = diag::MsgSpan::span_only(param.span);
                    secondary_msgs.push(msg_span);
                }

                if x.missing_ret_ty {
                    // point `->`:
                    let span = {
                        let ast = proc.ast(db);
                        let range = ast.right_arrow().unwrap().text_range();
                        Span::from_rowan_range(range)
                    };

                    let msg = "missing return type".to_string();
                    let msg_span = diag::MsgSpan::new(span, msg);
                    secondary_msgs.push(msg_span);
                }

                diag::render_multi_msg(
                    db.base(),
                    self,
                    input_file,
                    src_context,
                    primary_span,
                    secondary_msgs,
                )
            }
        }
    }
}
