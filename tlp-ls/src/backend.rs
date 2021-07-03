mod bufs;

use tower_lsp::{jsonrpc::Result, lsp_types::*, Client};

use self::bufs::{Buffer, BufferSync};

// TODO: cancelable

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
    pub async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Incremental,
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
            },
        })
    }

    pub async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::Info, "initialized!")
            .await;
    }

    pub async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    pub async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::Info, "workspace folders changed!")
            .await;
    }

    pub async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::Info, "configuration changed!")
            .await;
    }

    pub async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::Info, "watched files have changed!")
            .await;
    }

    pub async fn did_open(&mut self, p: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::Info, "file opened!")
            .await;

        let uri = p.text_document.uri.clone();
        let buf = Buffer::new(uri.clone(), p.text_document.text);

        self.bufs.insert(uri.clone(), buf);

        self.analyze_uri(&uri).await;
    }

    pub async fn did_change(&mut self, p: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::Info, "file changed!")
            .await;

        let uri = &p.text_document.uri;

        let change = match p.content_changes.into_iter().last() {
            Some(change) => change,
            None => return,
        };

        self.client
            .log_message(MessageType::Info, "sync buffer!")
            .await;

        self.bufs.set_buf(&uri, change.text);
    }

    pub async fn did_save(&mut self, p: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::Info, "file saved!")
            .await;

        if p.text.is_some() {
            self.client
                .log_message(MessageType::Info, "TODO: save file with text?")
                .await;
        } else {
            self.analyze_uri(&p.text_document.uri).await;
        }
    }

    pub async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::Info, "file closed!")
            .await;
    }

    pub async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }
}

async fn analyze_buf(client: &mut Client, buf: &Buffer) {
    client.log_message(MessageType::Info, "analyzing..").await;

    let diags = self::bufs::analyze(buf);

    if diags.is_empty() {
        client
            .log_message(MessageType::Info, "no diagnostics")
            .await;
        return;
    }

    client
        .log_message(MessageType::Info, "publish diagnostics")
        .await;

    client
        .publish_diagnostics(buf.uri().clone(), diags, None)
        .await;
}
