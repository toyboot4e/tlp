use std::collections::HashMap;

use tower_lsp::lsp_types as lty;

// use codespan_reporting as cr;

use tlp::syntax::{
    ast,
    span::{ByteLocation, ByteSpan, TextPos},
    validate::Validate,
};

#[derive(Debug, Clone)]
pub struct Buffer {
    uri: lty::Url,
    text: String,
}

impl Buffer {
    pub fn new(uri: lty::Url, text: String) -> Self {
        Self { uri, text }
    }

    pub fn uri(&self) -> &lty::Url {
        &self.uri
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn text_mut(&mut self) -> &mut String {
        &mut self.text
    }
}

impl Buffer {
    pub fn to_lsp_pos(&self, pos: TextPos) -> lty::Position {
        let loc = ByteLocation::from_pos(pos, &self.text);

        lty::Position {
            line: loc.ln as u32,
            character: loc.col as u32,
        }
    }

    pub fn to_lsp_range(&self, sp: ByteSpan) -> lty::Range {
        lty::Range {
            start: self.to_lsp_pos(sp.lo),
            end: self.to_lsp_pos(sp.hi),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BufferSync {
    synced: HashMap<lty::Url, Buffer>,
}

impl BufferSync {
    pub fn get(&self, uri: &lty::Url) -> Option<&Buffer> {
        self.synced.get(uri)
    }

    pub fn get_mut(&mut self, uri: &lty::Url) -> Option<&mut Buffer> {
        self.synced.get_mut(uri)
    }

    pub fn insert(&mut self, uri: lty::Url, buf: Buffer) {
        self.synced.insert(uri, buf);
    }
}

pub fn analyze(buf: &Buffer) -> Vec<lty::Diagnostic> {
    let mut diags = vec![];

    let ast::ParseResult { doc, errs } = ast::parse(&buf.text);

    for e in errs {
        let severity = lty::DiagnosticSeverity::ERROR;

        let range = buf.to_lsp_range(e.span());

        let diag = lty::Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: None,
            message: format!("{}", e),
            related_information: None,
            tags: None,
            data: None,
        };

        diags.push(diag);
    }

    let mut errs = vec![];
    doc.validate(&mut errs);

    for e in errs {
        let severity = lty::DiagnosticSeverity::ERROR;

        let range = buf.to_lsp_range(e.span());

        let diag = lty::Diagnostic {
            range,
            severity: Some(severity),
            code: None,
            code_description: None,
            source: None,
            message: format!("{}", e),
            related_information: None,
            tags: None,
            data: None,
        };

        diags.push(diag);
    }

    diags
}
