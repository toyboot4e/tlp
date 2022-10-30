//! LSP diagnostic type conversion

use base::{
    ln::LineTable,
    span::{LineColumn, Span},
};

use crate::util::diag;

pub trait IntoLspDiagnostic {
    fn into_lsp_type(self) -> lsp_types::Diagnostic;
}

impl diag::Severity {
    pub fn into_lsp_type(self) -> lsp_types::DiagnosticSeverity {
        use lsp_types::DiagnosticSeverity as D;

        match self {
            diag::Severity::Error => D::ERROR,
            diag::Severity::Warning => D::WARNING,
            diag::Severity::Note => D::INFORMATION,
            diag::Severity::Hint => D::HINT,
        }
    }
}

pub fn ln_col_to_lsp_position(ln_col: LineColumn) -> lsp_types::Position {
    lsp_types::Position {
        line: ln_col.line0(),
        character: ln_col.column0(),
    }
}

pub fn span_to_lsp_range(ln_tbl: &LineTable, span: Span) -> lsp_types::Range {
    let (start, end) = ln_tbl.line_column_span(span);

    lsp_types::Range {
        start: self::ln_col_to_lsp_position(start),
        end: self::ln_col_to_lsp_position(end),
    }
}

impl<'a> diag::Render<'a> {
    pub fn to_lsp_diagnostic(&self, ln_tbl: &LineTable) -> lsp_types::Diagnostic {
        let code = self
            .header
            .severity_display
            .code
            .map(|code| lsp_types::NumberOrString::String(code.to_string()));

        match &self.window {
            diag::Window::Primary(x) => {
                let range = self::span_to_lsp_range(ln_tbl, x.line_span);

                lsp_types::Diagnostic {
                    range,
                    severity: Some(x.severity.into_lsp_type()),
                    code,
                    code_description: None,
                    // TODO: what's this?
                    source: None,
                    message: self.header.msg.clone(),
                    // TODO: add related information
                    related_information: None,
                    tags: None,
                    data: None,
                }
            }
            // TODO: return multiple diagnostics
            diag::Window::Secondary(x) => {
                let x = &x.lines[0];
                let range = self::span_to_lsp_range(ln_tbl, x.line_span);

                lsp_types::Diagnostic {
                    range,
                    severity: Some(x.severity.into_lsp_type()),
                    code,
                    code_description: None,
                    // TODO: what's this?
                    source: None,
                    message: self.header.msg.clone(),
                    // TODO: add related information
                    related_information: None,
                    tags: None,
                    data: None,
                }
            }
        }
    }
}
