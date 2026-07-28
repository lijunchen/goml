ci: verify-release-tools verify-version test-bootstrap-stage0 verify-bootstrap test-selfhost vscode-ext

verify-release-tools:
    bash -n tools/release/release.sh tools/release/test.sh tools/release/lsp_smoke.sh
    bash tools/release/test.sh

verify-version:
    bash tools/release/release.sh check-version "$(cat VERSION)"

set-version version:
    bash tools/release/release.sh set-version "{{version}}"
    cd editors/vscode && npm version --no-git-tag-version "{{version}}"
    just verify-version

set-bootstrap-stage0 version sha256:
    bash tools/release/release.sh set-stage0 "{{version}}" "{{sha256}}"
    just bootstrap-stage0

test-bootstrap-stage0:
    bash -n bootstrap/bootstrap.sh

bootstrap-stage0:
    bash bootstrap/bootstrap.sh bootstrap/stage0.env _bootstrap/stage0

bootstrap-stage1: bootstrap-stage0
    mkdir -p bin/stage1
    cd gomlc && ../_bootstrap/stage0/bin/goml build --target-dir _bootstrap/stage1 --compiler ../_bootstrap/stage0/bin/gomlc
    cp gomlc/_bootstrap/stage1/bin/cmd/gomlc/gomlc bin/stage1/gomlc
    cp gomlc/_bootstrap/stage1/bin/cmd/gomllsp/gomllsp bin/stage1/gomllsp
    cd goml && ../_bootstrap/stage0/bin/goml build --target-dir _bootstrap/stage1 --compiler ../bin/stage1/gomlc
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

bootstrap: verify-bootstrap

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
    cd editors/vscode && code --install-extension "goml-$(cat ../../VERSION).vsix"

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
    cd gomlc && GOML_TEST_GOML=../bin/stage1/goml GOML_TEST_GOMLC=../bin/stage1/gomlc ../bin/stage1/goml test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 10m

test-bootstrap-driver: bootstrap-stage1
    cd goml && GOML_TEST_GOML=../bin/stage1/goml GOML_TEST_GOMLC=../bin/stage1/gomlc ../bin/stage1/goml test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 10m

test-bootstrap-pipeline: bootstrap-stage1
    cd gomlc && ../bin/stage1/goml test pipeline_test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 10m

test-bootstrap-compiler: bootstrap-stage1
    cd gomlc && GOML_TEST_GOML=../bin/stage1/goml GOML_TEST_GOMLC=../bin/stage1/gomlc ../bin/stage1/goml test compiler_test --compiler ../bin/stage1/gomlc --jobs 4 --timeout 10m

test-bootstrap-lsp: bootstrap-stage1
    cd gomlc && ../bin/stage1/goml test query --compiler ../bin/stage1/gomlc --jobs 1
    cd gomlc && ../bin/stage1/goml test lsp --compiler ../bin/stage1/gomlc --jobs 1

update-golden:
    env UPDATE_EXPECT=1 just test-bootstrap-pipeline
    env UPDATE_EXPECT=1 just test-bootstrap-compiler

verify-golden:
    just test-bootstrap-pipeline
    just test-bootstrap-compiler
