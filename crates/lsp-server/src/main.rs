use std::{collections::HashMap, path::PathBuf};

use dashmap::DashMap;
use lsp_server::{Document, handlers};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing::info;

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: DashMap<Url, Document>,
    root_uri: tokio::sync::RwLock<Option<Url>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: DashMap::new(),
            root_uri: tokio::sync::RwLock::new(None),
        }
    }

    fn get_file_path(&self, uri: &Url) -> Option<PathBuf> {
        uri.to_file_path().ok()
    }

    fn source_overrides(&self) -> HashMap<PathBuf, String> {
        self.documents
            .iter()
            .filter_map(|entry| {
                entry
                    .key()
                    .to_file_path()
                    .ok()
                    .map(|path| (path, entry.value().content.clone()))
            })
            .collect()
    }

    fn document_content(&self, uri: &Url) -> Option<String> {
        self.documents.get(uri).map(|doc| doc.content.clone())
    }

    async fn publish_diagnostics(&self, uri: Url) {
        let Some(content) = self.document_content(&uri) else {
            return;
        };
        let Some(path) = self.get_file_path(&uri) else {
            return;
        };

        let doc = Document::new(content.clone());
        let diagnostics = handlers::get_diagnostics_with_overrides(
            &path,
            &content,
            &doc,
            &self.source_overrides(),
        );
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn publish_all_diagnostics(&self) {
        let uris = self
            .documents
            .iter()
            .map(|entry| entry.key().clone())
            .collect::<Vec<_>>();
        for uri in uris {
            self.publish_diagnostics(uri).await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(root_uri) = params.root_uri {
            *self.root_uri.write().await = Some(root_uri);
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                position_encoding: Some(PositionEncodingKind::UTF16),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        ":".to_string(),
                        " ".to_string(),
                    ]),
                    resolve_provider: Some(false),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: Some(vec![",".to_string()]),
                    ..Default::default()
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "goml-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        info!("goml-lsp server initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        self.documents.insert(uri.clone(), Document::new(content));
        self.publish_all_diagnostics().await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.documents
                .insert(uri.clone(), Document::new(change.text));
            self.publish_all_diagnostics().await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.remove(&uri);
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
        self.publish_all_diagnostics().await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let _ = params;
        self.publish_all_diagnostics().await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(content) = self.document_content(uri) else {
            return Ok(None);
        };
        let Some(path) = self.get_file_path(uri) else {
            return Ok(None);
        };

        Ok(handlers::hover_with_overrides(
            &path,
            &content,
            position,
            &self.source_overrides(),
        ))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let Some(content) = self.document_content(uri) else {
            return Ok(None);
        };
        let Some(path) = self.get_file_path(uri) else {
            return Ok(None);
        };

        Ok(handlers::completion_with_overrides(
            &path,
            &content,
            position,
            &self.source_overrides(),
        ))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(content) = self.document_content(uri) else {
            return Ok(None);
        };
        let Some(path) = self.get_file_path(uri) else {
            return Ok(None);
        };

        Ok(handlers::signature_help_with_overrides(
            &path,
            &content,
            position,
            &self.source_overrides(),
        ))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let range = params.range;

        let Some(content) = self.document_content(uri) else {
            return Ok(None);
        };
        let Some(path) = self.get_file_path(uri) else {
            return Ok(None);
        };

        let doc = Document::new(content.clone());
        Ok(handlers::inlay_hints_with_overrides(
            &path,
            &content,
            range,
            &doc,
            &self.source_overrides(),
        ))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let Some(content) = self.document_content(uri) else {
            return Ok(None);
        };
        let Some(path) = self.get_file_path(uri) else {
            return Ok(None);
        };

        let doc = Document::new(content.clone());
        Ok(handlers::goto_definition_with_overrides(
            uri,
            &path,
            &content,
            position,
            &doc,
            &self.source_overrides(),
        ))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let Some(content) = self.document_content(&uri) else {
            return Ok(None);
        };
        let Some(path) = self.get_file_path(&uri) else {
            return Ok(None);
        };
        let doc = Document::new(content.clone());
        Ok(Some(handlers::code_lenses(&uri, &path, &content, &doc)))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(handlers::code_actions(&params.context))
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .with_writer(std::io::stderr)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
