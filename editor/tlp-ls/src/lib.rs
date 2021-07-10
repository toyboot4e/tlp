/*!
toylisp LSP implementation (server side)
*/

mod backend;

use std::sync::Arc;

use tokio::sync::Mutex;

use tower_lsp::{
    jsonrpc::Result,
    lsp_types::*,
    {Client, LanguageServer},
};

use self::backend::Inner;

#[derive(Debug)]
pub struct Backend {
    inner: Arc<Mutex<Inner>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Inner::new(client))),
        }
    }
}

/// Delegate the implementation to `&mut Inner`
#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, p: InitializeParams) -> Result<InitializeResult> {
        self.inner.lock().await.initialize(p).await
    }

    async fn initialized(&self, p: InitializedParams) {
        self.inner.lock().await.initialized(p).await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.inner.lock().await.shutdown().await
    }

    async fn did_change_workspace_folders(&self, p: DidChangeWorkspaceFoldersParams) {
        self.inner
            .lock()
            .await
            .did_change_workspace_folders(p)
            .await;
    }

    async fn did_change_configuration(&self, p: DidChangeConfigurationParams) {
        self.inner.lock().await.did_change_configuration(p).await;
    }

    async fn did_change_watched_files(&self, p: DidChangeWatchedFilesParams) {
        self.inner.lock().await.did_change_watched_files(p).await;
    }

    async fn did_open(&self, p: DidOpenTextDocumentParams) {
        self.inner.lock().await.did_open(p).await;
    }

    async fn did_change(&self, p: DidChangeTextDocumentParams) {
        self.inner.lock().await.did_change(p).await;
    }

    async fn did_save(&self, p: DidSaveTextDocumentParams) {
        self.inner.lock().await.did_save(p).await;
    }

    async fn did_close(&self, p: DidCloseTextDocumentParams) {
        self.inner.lock().await.did_close(p).await;
    }

    async fn completion(&self, p: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.inner.lock().await.completion(p).await
    }
}
