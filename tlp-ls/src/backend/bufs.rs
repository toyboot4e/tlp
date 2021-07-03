use std::collections::HashMap;

use tower_lsp::lsp_types as lty;

// use codespan_reporting as cr;

use tlp::syntax::{
    cst::parse,
    span::{self, ByteSpan, TextPos},
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
}

impl Buffer {
    pub fn to_lsp_pos(&self, pos: TextPos) -> lty::Position {
        let (ln, col) = span::ln_col(pos, &self.text);

        lty::Position {
            line: ln as u32,
            character: col as u32,
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

    pub fn insert(&mut self, uri: lty::Url, buf: Buffer) {
        self.synced.insert(uri, buf);
    }

    pub fn set_buf(&mut self, uri: &lty::Url, new_text: String) {
        return; // TODO:
        let buf = match self.synced.get_mut(uri) {
            Some(buf) => buf,
            None => return,
        };
        buf.text = new_text;
    }
}

pub fn analyze(buf: &Buffer) -> Vec<lty::Diagnostic> {
    let mut diags = vec![];

    let (_tree, errs) = parse::from_str(&buf.text);

    for e in &errs {
        let severity = lty::DiagnosticSeverity::Error;

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
