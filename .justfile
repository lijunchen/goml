ci: verify-bootstrap test-selfhost vscode-ext

build-stage0:
    mkdir -p bin/stage0
    go build -trimpath -o bin/stage0/gomlc stage0/gomlc/gomlc.go
    go build -trimpath -o bin/stage0/goml stage0/goml/goml.go

bootstrap-stage1: build-stage0
    mkdir -p bin/stage1
    cd gomlc && ../bin/stage0/goml build --target-dir _bootstrap/stage1 --compiler ../bin/stage0/gomlc
    cp gomlc/_bootstrap/stage1/bin/cmd/gomlc/gomlc bin/stage1/gomlc
    cp gomlc/_bootstrap/stage1/bin/cmd/gomllsp/gomllsp bin/stage1/gomllsp
    cd goml && ../bin/stage0/goml build --target-dir _bootstrap/stage1 --compiler ../bin/stage1/gomlc
    cp goml/_bootstrap/stage1/bin/cmd/goml/goml bin/stage1/goml

bootstrap-stage2: bootstrap-stage1
    mkdir -p bin/stage2
    cd gomlc && ../bin/stage1/goml build --target-dir _bootstrap/stage2 --compiler ../bin/stage1/gomlc
    cp gomlc/_bootstrap/stage2/bin/cmd/gomlc/gomlc bin/stage2/gomlc
    cp gomlc/_bootstrap/stage2/bin/cmd/gomllsp/gomllsp bin/stage2/gomllsp
    cd goml && ../bin/stage1/goml build --target-dir _bootstrap/stage2 --compiler ../bin/stage1/gomlc
    cp goml/_bootstrap/stage2/bin/cmd/goml/goml bin/stage2/goml

verify-fixed-point: bootstrap-stage2
    diff -ru --exclude='*.goml-*-fingerprint' gomlc/_bootstrap/stage1/build/pkg gomlc/_bootstrap/stage2/build/pkg
    diff -ru --exclude='*.goml-*-fingerprint' goml/_bootstrap/stage1/build/pkg goml/_bootstrap/stage2/build/pkg

verify-bootstrap: verify-fixed-point
    cmp stage0/gomlc/gomlc.go gomlc/_bootstrap/stage2/build/pkg/gomlc/cmd/gomlc/goml_generated.go
    cmp stage0/goml/goml.go goml/_bootstrap/stage2/build/pkg/gomlang/bootstrap_goml/cmd/goml/goml_generated.go

bootstrap: verify-bootstrap

regenerate-stage0: bootstrap-stage2
    cp gomlc/_bootstrap/stage2/build/pkg/gomlc/cmd/gomlc/goml_generated.go stage0/gomlc/gomlc.go
    cp goml/_bootstrap/stage2/build/pkg/gomlang/bootstrap_goml/cmd/goml/goml_generated.go stage0/goml/goml.go
    just verify-fixed-point

build-lsp: bootstrap-stage1

install-lsp: bootstrap-stage1
    mkdir -p editors/vscode/bin
    cp bin/stage1/gomllsp editors/vscode/bin/gomllsp
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
    cp bin/stage1/gomlc "${GOML_HOME:-$HOME/.goml}/bin/gomlc"
    cp bin/stage1/goml "${GOML_HOME:-$HOME/.goml}/bin/goml"
    cp bin/stage1/gomllsp "${GOML_HOME:-$HOME/.goml}/bin/gomllsp"
    mkdir -p "${GOML_HOME:-$HOME/.goml}/lib/std"
    cp -R stdlib/std/. "${GOML_HOME:-$HOME/.goml}/lib/std/"
    cp stdlib/builtin_prelude.gom "${GOML_HOME:-$HOME/.goml}/lib/builtin_prelude.gom"

test-selfhost: test-bootstrap-all test-bootstrap-driver

test-bootstrap-all: bootstrap-stage1
    cd gomlc && GOML_TEST_GOML=../bin/stage1/goml GOML_TEST_GOMLC=../bin/stage1/gomlc ../bin/stage1/goml test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 2m

test-bootstrap-driver: bootstrap-stage1
    cd goml && GOML_TEST_GOML=../bin/stage1/goml GOML_TEST_GOMLC=../bin/stage1/gomlc ../bin/stage1/goml test --compiler ../bin/stage1/gomlc --jobs 4

test-bootstrap-pipeline: bootstrap-stage1
    cd gomlc && ../bin/stage1/goml test pipeline_test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 2m

test-bootstrap-compiler: bootstrap-stage1
    cd gomlc && GOML_TEST_GOML=../bin/stage1/goml GOML_TEST_GOMLC=../bin/stage1/gomlc ../bin/stage1/goml test compiler_test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 2m

test-bootstrap-lsp: bootstrap-stage1
    cd gomlc && ../bin/stage1/goml test query --compiler ../bin/stage1/gomlc --jobs 1
    cd gomlc && ../bin/stage1/goml test lsp --compiler ../bin/stage1/gomlc --jobs 1

update-golden:
    env UPDATE_EXPECT=1 just test-bootstrap-pipeline
    env UPDATE_EXPECT=1 just test-bootstrap-compiler

verify-golden:
    just test-bootstrap-pipeline
    just test-bootstrap-compiler
