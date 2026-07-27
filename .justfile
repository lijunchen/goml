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

generate-stage0-compiler-from-rust:
    cargo build -p goml -p gomlc
    cd bootstrap && ../target/debug/goml build --target-dir _artifact/stage0-rust --compiler ../target/debug/gomlc
    cd bootstrap && ../target/debug/goml build --target-dir _artifact/stage0-selfhost --compiler _artifact/stage0-rust/bin/cmd/gomlc/gomlc
    cd bootstrap && ../target/debug/goml build --target-dir _artifact/stage0-fixed --compiler _artifact/stage0-selfhost/bin/cmd/gomlc/gomlc
    cmp bootstrap/_artifact/stage0-selfhost/build/pkg/gomlc/cmd/gomlc/goml_generated.go bootstrap/_artifact/stage0-fixed/build/pkg/gomlc/cmd/gomlc/goml_generated.go
    mkdir -p stage0/gomlc
    cp bootstrap/_artifact/stage0-fixed/build/pkg/gomlc/cmd/gomlc/goml_generated.go stage0/gomlc/gomlc.go
    go build -trimpath -o bootstrap/_artifact/stage0-gomlc stage0/gomlc/gomlc.go

build-lsp:
    cargo build -p lsp-server

install-lsp:
    cargo build -p lsp-server
    mkdir -p editors/vscode/bin
    cp target/debug/goml-lsp editors/vscode/bin/

build-bootstrap-lsp:
    cargo build -p goml -p gomlc
    cd bootstrap && ../target/debug/goml build --compiler ../target/debug/gomlc
    cd bootstrap && ../target/debug/goml build --compiler _artifact/bin/cmd/gomlc/gomlc

test-bootstrap-lsp:
    cargo build -p goml -p gomlc
    cd bootstrap && ../target/debug/goml test query --compiler ../target/debug/gomlc --jobs 1
    cd bootstrap && ../target/debug/goml test lsp --compiler ../target/debug/gomlc --jobs 1
    just build-bootstrap-lsp

install-bootstrap-lsp:
    just build-bootstrap-lsp
    mkdir -p editors/vscode/bin
    cp bootstrap/_artifact/bin/cmd/gomllsp/gomllsp editors/vscode/bin/
    cp crates/compiler/src/builtin_prelude.gom editors/vscode/bin/
    rm -rf editors/vscode/bin/lib/std
    mkdir -p editors/vscode/bin/lib
    cp -R stdlib/std editors/vscode/bin/lib/std

vscode-ext:
    just install-lsp
    cd editors/vscode && npm install && npm run compile

vscode-ext-bootstrap:
    just install-bootstrap-lsp
    cd editors/vscode && npm install && npm run compile

package-vscode-ext:
    cd editors/vscode && npx @vscode/vsce package --allow-missing-repository --skip-license

install-vscode-ext:
    cd editors/vscode && npx @vscode/vsce package --allow-missing-repository --skip-license && code --install-extension *.vsix

install:
    home="${GOML_HOME:-$HOME/.goml}"; cargo install --path ./crates/gomlc --debug --offline --root "$home" --force --locked; cargo install --path ./crates/goml --debug --offline --root "$home" --force --locked; rm -rf "$home/lib/std"; mkdir -p "$home/lib"; cp -R stdlib/std "$home/lib/std"

install-bootstrap:
    cargo build -p goml -p gomlc
    cd bootstrap && ../target/debug/goml build --compiler ../target/debug/gomlc
    cd bootstrap-goml && ../target/debug/goml build --compiler ../bootstrap/_artifact/bin/cmd/gomlc/gomlc
    goml_home_dir="${GOML_HOME:-$HOME/.goml}"; mkdir -p "$goml_home_dir/bin"; cp bootstrap/_artifact/bin/cmd/gomlc/gomlc "$goml_home_dir/bin/gomlc"; cp bootstrap-goml/_artifact/bin/cmd/goml/goml "$goml_home_dir/bin/goml"; rm -rf "$goml_home_dir/lib/std"; mkdir -p "$goml_home_dir/lib"; cp -R stdlib/std "$goml_home_dir/lib/std"; cp crates/compiler/src/builtin_prelude.gom "$goml_home_dir/lib/"

test-bootstrap-goml:
    cargo build -p goml -p gomlc
    cd bootstrap-goml && ../target/debug/goml build --compiler ../target/debug/gomlc
    cd bootstrap-goml && ../target/debug/goml test --compiler ../target/debug/gomlc --jobs 1

test-bootstrap-self: _bootstrap-check _bootstrap-test

test-bootstrap-self-full: _bootstrap-check _bootstrap-build _bootstrap-test

_bootstrap-check:
    cd bootstrap-goml && _artifact/bin/cmd/goml/goml check --compiler ../bootstrap/_artifact/bin/cmd/gomlc/gomlc

_bootstrap-build:
    cd bootstrap-goml && _artifact/bin/cmd/goml/goml build --compiler ../bootstrap/_artifact/bin/cmd/gomlc/gomlc

_bootstrap-test:
    cd bootstrap-goml && _artifact/bin/cmd/goml/goml test --compiler ../bootstrap/_artifact/bin/cmd/gomlc/gomlc --jobs 1

test-bootstrap-pipeline:
    cargo build -p goml -p gomlc
    cd bootstrap && ../target/debug/goml test pipeline_test --compiler ../target/debug/gomlc --jobs 1

test-bootstrap-compiler:
    cargo build -p goml -p gomlc
    cd bootstrap && ../target/debug/goml build --compiler ../target/debug/gomlc
    cd bootstrap-goml && ../target/debug/goml build --compiler ../bootstrap/_artifact/bin/cmd/gomlc/gomlc
    cd bootstrap && ../bootstrap-goml/_artifact/bin/cmd/goml/goml test compiler_test --compiler _artifact/bin/cmd/gomlc/gomlc --jobs 4

install-lsp-suite:
    just install-lsp
    just install-vscode-ext
