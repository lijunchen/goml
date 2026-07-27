ci: verify-bootstrap test-selfhost vscode-ext

build-stage0:
    mkdir -p _bootstrap/bin
    go build -trimpath -o _bootstrap/bin/gomlc stage0/gomlc/gomlc.go
    go build -trimpath -o _bootstrap/bin/goml stage0/goml/goml.go

bootstrap-stage1: build-stage0
    cd bootstrap && ../_bootstrap/bin/goml build --target-dir _bootstrap/stage1 --compiler ../_bootstrap/bin/gomlc
    cd bootstrap-goml && ../_bootstrap/bin/goml build --target-dir _bootstrap/stage1 --compiler ../bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc

bootstrap-stage2: bootstrap-stage1
    cd bootstrap && ../bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml build --target-dir _bootstrap/stage2 --compiler _bootstrap/stage1/bin/cmd/gomlc/gomlc
    cd bootstrap-goml && _bootstrap/stage1/bin/cmd/goml/goml build --target-dir _bootstrap/stage2 --compiler ../bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc

verify-fixed-point: bootstrap-stage2
    diff -ru --exclude='*.goml-*-fingerprint' bootstrap/_bootstrap/stage1/build/pkg bootstrap/_bootstrap/stage2/build/pkg
    diff -ru --exclude='*.goml-*-fingerprint' bootstrap-goml/_bootstrap/stage1/build/pkg bootstrap-goml/_bootstrap/stage2/build/pkg

verify-bootstrap: verify-fixed-point
    cmp stage0/gomlc/gomlc.go bootstrap/_bootstrap/stage2/build/pkg/gomlc/cmd/gomlc/goml_generated.go
    cmp stage0/goml/goml.go bootstrap-goml/_bootstrap/stage2/build/pkg/gomlang/bootstrap_goml/cmd/goml/goml_generated.go

bootstrap: verify-bootstrap

regenerate-stage0: bootstrap-stage2
    cp bootstrap/_bootstrap/stage2/build/pkg/gomlc/cmd/gomlc/goml_generated.go stage0/gomlc/gomlc.go
    cp bootstrap-goml/_bootstrap/stage2/build/pkg/gomlang/bootstrap_goml/cmd/goml/goml_generated.go stage0/goml/goml.go
    just verify-fixed-point

build-lsp: bootstrap-stage1

install-lsp: bootstrap-stage1
    mkdir -p editors/vscode/bin
    cp bootstrap/_bootstrap/stage1/bin/cmd/gomllsp/gomllsp editors/vscode/bin/gomllsp
    cp stdlib/builtin_prelude.gom editors/vscode/bin/builtin_prelude.gom
    mkdir -p editors/vscode/bin/lib/std
    cp -R stdlib/std/. editors/vscode/bin/lib/std/

vscode-ext: install-lsp
    cd editors/vscode && npm install
    cd editors/vscode && npm run compile

package-vscode-ext: vscode-ext
    cd editors/vscode && npx @vscode/vsce package --allow-missing-repository --skip-license

install-vscode-ext: package-vscode-ext
    cd editors/vscode && code --install-extension goml-0.1.0.vsix

install: bootstrap-stage1
    mkdir -p "${GOML_HOME:-$HOME/.goml}/bin"
    cp bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc "${GOML_HOME:-$HOME/.goml}/bin/gomlc"
    cp bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml "${GOML_HOME:-$HOME/.goml}/bin/goml"
    cp bootstrap/_bootstrap/stage1/bin/cmd/gomllsp/gomllsp "${GOML_HOME:-$HOME/.goml}/bin/gomllsp"
    mkdir -p "${GOML_HOME:-$HOME/.goml}/lib/std"
    cp -R stdlib/std/. "${GOML_HOME:-$HOME/.goml}/lib/std/"
    cp stdlib/builtin_prelude.gom "${GOML_HOME:-$HOME/.goml}/lib/builtin_prelude.gom"

test-selfhost: test-bootstrap-all test-bootstrap-driver

test-bootstrap-all: bootstrap-stage1
    cd bootstrap && ../bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml test --compiler _bootstrap/stage1/bin/cmd/gomlc/gomlc --jobs 4

test-bootstrap-driver: bootstrap-stage1
    cd bootstrap-goml && GOML_TEST_GOML=_bootstrap/stage1/bin/cmd/goml/goml GOML_TEST_GOMLC=../bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc _bootstrap/stage1/bin/cmd/goml/goml test --compiler ../bootstrap/_bootstrap/stage1/bin/cmd/gomlc/gomlc --jobs 1

test-bootstrap-pipeline: bootstrap-stage1
    cd bootstrap && ../bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml test pipeline_test --compiler _bootstrap/stage1/bin/cmd/gomlc/gomlc --jobs 1

test-bootstrap-compiler: bootstrap-stage1
    cd bootstrap && ../bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml test compiler_test --compiler _bootstrap/stage1/bin/cmd/gomlc/gomlc --jobs 4

test-bootstrap-lsp: bootstrap-stage1
    cd bootstrap && ../bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml test query --compiler _bootstrap/stage1/bin/cmd/gomlc/gomlc --jobs 1
    cd bootstrap && ../bootstrap-goml/_bootstrap/stage1/bin/cmd/goml/goml test lsp --compiler _bootstrap/stage1/bin/cmd/gomlc/gomlc --jobs 1

update-golden:
    env UPDATE_EXPECT=1 just test-bootstrap-pipeline
    env UPDATE_EXPECT=1 just test-bootstrap-compiler

verify-golden:
    just test-bootstrap-pipeline
    just test-bootstrap-compiler
