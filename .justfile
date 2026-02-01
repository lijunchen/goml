clippy:
    cargo clippy --all-targets --all-features --locked -- -D warnings

build-wasm:
    wasm-pack build ./crates/wasm-app

start:
    wasm-pack build ./crates/wasm-app
    cd webapp && rm -rf node_modules && pnpm install && pnpm run dev

ci:
    cargo check
    cargo test
    cargo fmt
    just clippy

build-lsp:
    cargo build -p lsp-server --release

install-lsp:
    cargo build -p lsp-server --release
    mkdir -p editors/vscode/bin
    cp target/release/goml-lsp editors/vscode/bin/

vscode-ext:
    just install-lsp
    cd editors/vscode && npm install && npm run compile
