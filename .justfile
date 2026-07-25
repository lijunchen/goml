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
    cargo build -p lsp-server

install-lsp:
    cargo build -p lsp-server
    mkdir -p editors/vscode/bin
    cp target/debug/goml-lsp editors/vscode/bin/

vscode-ext:
    just install-lsp
    cd editors/vscode && npm install && npm run compile

package-vscode-ext:
    cd editors/vscode && npx @vscode/vsce package --allow-missing-repository --skip-license

install-vscode-ext:
    cd editors/vscode && npx @vscode/vsce package --allow-missing-repository --skip-license && code --install-extension *.vsix

install:
    home="${GOML_HOME:-$HOME/.goml}"; cargo install --path ./crates/gomlc --debug --offline --root "$home" --force --locked; cargo install --path ./crates/goml --debug --offline --root "$home" --force --locked; rm -rf "$home/lib/std"; mkdir -p "$home/lib"; cp -R stdlib/std "$home/lib/std"

install-bootstrap:
    cargo build -p goml -p gomlc
    target/debug/goml build bootstrap/cmd/gomlc --compiler target/debug/gomlc
    target/debug/goml build bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc
    home="${GOML_HOME:-$HOME/.goml}"; mkdir -p "$home/bin"; cp bootstrap/_artifact/bin/cmd/gomlc/gomlc "$home/bin/gomlc"; cp bootstrap-goml/_artifact/bin/cmd/goml/goml "$home/bin/goml"; rm -rf "$home/lib/std"; mkdir -p "$home/lib"; cp -R stdlib/std "$home/lib/std"

test-bootstrap-goml:
    cargo build -p goml -p gomlc
    target/debug/goml build bootstrap-goml/cmd/goml --compiler target/debug/gomlc
    target/debug/goml test bootstrap-goml/cmd/goml --compiler target/debug/gomlc --jobs 1

test-bootstrap-self: _bootstrap-check _bootstrap-test

test-bootstrap-self-full: _bootstrap-check _bootstrap-build _bootstrap-test

_bootstrap-check:
    bootstrap-goml/_artifact/bin/cmd/goml/goml check bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc

_bootstrap-build:
    bootstrap-goml/_artifact/bin/cmd/goml/goml build bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc

_bootstrap-test:
    bootstrap-goml/_artifact/bin/cmd/goml/goml test bootstrap-goml/cmd/goml --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc --jobs 1

test-bootstrap-pipeline:
    cargo build -p goml -p gomlc
    target/debug/goml test bootstrap/pipeline_test --compiler target/debug/gomlc --jobs 1

install-lsp-suite:
    just install-lsp
    just install-vscode-ext
