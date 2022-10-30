import * as path from "path";
import { workspace, ExtensionContext } from "vscode";

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from "vscode-languageclient";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    let lang = "tlp";
    // let cmd = "tlp-ls";
    let cmd = "/Users/toy/dev/rs/tlp/editor/tlp-ls/target/debug/tlp-ls";

    let serverOptions: ServerOptions = {
        run: { command: cmd },
        debug: { command: cmd, args: ["--debug"] },
    };

    // watch toylisp files
    let clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: "file", language: "toylisp" }],
        // ?
        initializationOptions: workspace.getConfiguration("toylisp"),
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher("*.tlp"),
        },
    };

    client = new LanguageClient(
        "tlp",
        "toylisp language server",
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
