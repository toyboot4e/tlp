use tlp_ls::Backend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(Backend::new);

    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
