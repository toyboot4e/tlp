mod bufs;
mod tokens;

use tlp::syntax::span::LineColumn;

use tower_lsp::{jsonrpc::Result, lsp_types::*, Client};

use self::bufs::{Buffer, BufferSync};

// TODO: cancelable. progress option?

pub const LSP_NAME: &'static str = "tlp-ls";

#[derive(Debug)]
pub struct Inner {
    client: Client,
    bufs: BufferSync,
}

impl Inner {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            bufs: BufferSync::default(),
        }
    }
}

impl Inner {
    async fn analyze_uri(&mut self, uri: &Url) {
        let buf = match self.bufs.get(uri) {
            Some(buf) => buf,
            None => return,
        };
        self::analyze_buf(&mut self.client, buf).await;
    }
}

/// LSP API
impl Inner {
    pub async fn initialize(&self, p: InitializeParams) -> Result<InitializeResult> {
        let meta = ServerInfo {
            name: LSP_NAME.to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        };

        let cap = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string()]),
                work_done_progress_options: Default::default(),
                all_commit_characters: None,
            }),
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(OneOf::Left(true)),
                }),
                file_operations: None,
            }),
            ..ServerCapabilities::default()
        };

        let mut res = InitializeResult {
            server_info: Some(meta),
            capabilities: cap,
        };

        self::tokens::init(&p, &self.client, &mut res).await;

        Ok(res)
    }

    pub async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    pub async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    pub async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    pub async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    pub async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    pub async fn did_open(&mut self, p: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;

        let uri = p.text_document.uri.clone();
        let buf = Buffer::new(uri.clone(), p.text_document.text);

        self.bufs.insert(uri.clone(), buf);

        self.analyze_uri(&uri).await;
    }

    /// Process incremental text change
    pub async fn did_change(&mut self, p: DidChangeTextDocumentParams) {
        let uri = &p.text_document.uri;

        let buf = match self.bufs.get_mut(uri) {
            Some(buf) => buf,
            None => {
                self.client
                    .log_message(
                        MessageType::INFO,
                        format!("unable to find document in cache: {}", uri),
                    )
                    .await;
                return;
            }
        };

        let text = buf.text_mut();

        self.client
            .log_message(MessageType::INFO, "file changed!")
            .await;

        // TODO: maybe use codespan and codespan_lsp
        for change in p.content_changes.into_iter() {
            let rng = change
                .range
                .clone()
                .expect("incremental document change expected");

            let lo = ByteLocation::from_view(
                rng.start.line as usize,
                rng.start.character as usize,
                text,
            )
            .unwrap()
            .to_pos();

            let hi =
                ByteLocation::from_view(rng.end.line as usize, rng.end.character as usize, text)
                    .unwrap()
                    .to_pos();

            text.replace_range(lo..hi, &change.text);
        }
    }

    pub async fn did_save(&mut self, p: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;

        if p.text.is_some() {
            self.client
                .log_message(MessageType::INFO, "TODO: save file with text?")
                .await;
        } else {
            self.analyze_uri(&p.text_document.uri).await;
        }
    }

    pub async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    pub async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    pub async fn semantic_tokens_full(
        &self,
        p: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        self.client
            .log_message(MessageType::INFO, "semantic tokens full")
            .await;

        let uri = &p.text_document.uri;

        // TODO: no unwrap
        let buf = self.bufs.get(uri).unwrap();
        let sts = tokens::highlight_full(buf);

        Ok(Some(SemanticTokensResult::Tokens(sts)))
    }
}

async fn analyze_buf(client: &mut Client, buf: &Buffer) {
    client.log_message(MessageType::INFO, "analyzing..").await;

    let diags = self::bufs::analyze(buf);

    if diags.is_empty() {
        client
            .log_message(MessageType::INFO, "no diagnostics")
            .await;

        // clear diagnostics
        client
            .publish_diagnostics(buf.uri().clone(), vec![], None)
            .await;

        return;
    }

    client
        .log_message(MessageType::INFO, "publish diagnostics")
        .await;

    client
        .publish_diagnostics(buf.uri().clone(), diags, None)
        .await;
}
