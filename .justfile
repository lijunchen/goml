make:
    bash bootstrap/bootstrap.sh bootstrap/stage0.env _bootstrap/stage0
    mkdir -p bin/stage1
    cd gomlc && ../_bootstrap/stage0/bin/goml build --target-dir _bootstrap/stage1 --compiler ../_bootstrap/stage0/bin/gomlc
    cp gomlc/_bootstrap/stage1/bin/cmd/gomlc/gomlc bin/stage1/gomlc
    cp gomlc/_bootstrap/stage1/bin/cmd/gomlfmt/gomlfmt bin/stage1/gomlfmt
    cp gomlc/_bootstrap/stage1/bin/cmd/gomllsp/gomllsp bin/stage1/gomllsp
    cd goml && ../_bootstrap/stage0/bin/goml build --target-dir _bootstrap/stage1 --compiler ../bin/stage1/gomlc
    cp goml/_bootstrap/stage1/bin/cmd/goml/goml bin/stage1/goml
    mkdir -p bin/stage2
    cd gomlc && ../bin/stage1/goml build --target-dir _bootstrap/stage2 --compiler ../bin/stage1/gomlc
    cp gomlc/_bootstrap/stage2/bin/cmd/gomlc/gomlc bin/stage2/gomlc
    cp gomlc/_bootstrap/stage2/bin/cmd/gomlfmt/gomlfmt bin/stage2/gomlfmt
    cp gomlc/_bootstrap/stage2/bin/cmd/gomllsp/gomllsp bin/stage2/gomllsp
    cd goml && ../bin/stage1/goml build --target-dir _bootstrap/stage2 --compiler ../bin/stage1/gomlc
    cp goml/_bootstrap/stage2/bin/cmd/goml/goml bin/stage2/goml

test: make
    cd gomlc && GOML_TEST_GOML=../bin/stage2/goml GOML_TEST_GOMLC=../bin/stage2/gomlc ../bin/stage2/goml test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    cd goml && GOML_TEST_GOML=../bin/stage2/goml GOML_TEST_GOMLC=../bin/stage2/gomlc ../bin/stage2/goml test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m

all: test

clean:
    rm -rf _artifact _bootstrap
    rm -rf gomlc/_artifact gomlc/_bootstrap
    rm -rf goml/_artifact goml/_bootstrap
    rm -rf bin/stage1 bin/stage2 bin/stage3
    rm -rf editors/vscode/bin

bootstrap:
    rm -rf gomlc/_bootstrap/stage1 gomlc/_bootstrap/stage2 gomlc/_bootstrap/stage3
    rm -rf goml/_bootstrap/stage1 goml/_bootstrap/stage2 goml/_bootstrap/stage3
    rm -rf bin/stage1 bin/stage2 bin/stage3
    just make
    mkdir -p bin/stage3
    cd gomlc && ../bin/stage2/goml build --target-dir _bootstrap/stage3 --compiler ../bin/stage2/gomlc
    cp gomlc/_bootstrap/stage3/bin/cmd/gomlc/gomlc bin/stage3/gomlc
    cp gomlc/_bootstrap/stage3/bin/cmd/gomlfmt/gomlfmt bin/stage3/gomlfmt
    cp gomlc/_bootstrap/stage3/bin/cmd/gomllsp/gomllsp bin/stage3/gomllsp
    cd goml && ../bin/stage2/goml build --target-dir _bootstrap/stage3 --compiler ../bin/stage2/gomlc
    cp goml/_bootstrap/stage3/bin/cmd/goml/goml bin/stage3/goml
    diff -ru --exclude='*.goml-*-fingerprint' gomlc/_bootstrap/stage2/build/pkg gomlc/_bootstrap/stage3/build/pkg
    diff -ru --exclude='*.goml-*-fingerprint' goml/_bootstrap/stage2/build/pkg goml/_bootstrap/stage3/build/pkg

ci: bootstrap
    bash -n tools/release/release.sh tools/release/test.sh tools/release/lsp_smoke.sh
    bash tools/release/test.sh
    bash tools/release/release.sh check-version "$(cat VERSION)"
    bash -n bootstrap/bootstrap.sh
    cd gomlc && GOML_TEST_GOML=../bin/stage2/goml GOML_TEST_GOMLC=../bin/stage2/gomlc ../bin/stage2/goml test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    cd goml && GOML_TEST_GOML=../bin/stage2/goml GOML_TEST_GOMLC=../bin/stage2/gomlc ../bin/stage2/goml test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    mkdir -p editors/vscode/bin
    cp bin/stage2/gomllsp editors/vscode/bin/gomllsp
    cp stdlib/builtin_prelude.gom editors/vscode/bin/builtin_prelude.gom
    mkdir -p editors/vscode/bin/lib/std
    cp -R stdlib/std/. editors/vscode/bin/lib/std/
    cd editors/vscode && npm install
    cd editors/vscode && npm run compile

set-version version:
    bash tools/release/release.sh set-version "{{version}}"
    cd editors/vscode && npm version --no-git-tag-version "{{version}}"
    bash tools/release/release.sh check-version "$(cat VERSION)"

set-bootstrap-stage0 version sha256:
    bash tools/release/release.sh set-stage0 "{{version}}" "{{sha256}}"
    bash bootstrap/bootstrap.sh bootstrap/stage0.env _bootstrap/stage0

vscode-ext: make
    mkdir -p editors/vscode/bin
    cp bin/stage2/gomllsp editors/vscode/bin/gomllsp
    cp stdlib/builtin_prelude.gom editors/vscode/bin/builtin_prelude.gom
    mkdir -p editors/vscode/bin/lib/std
    cp -R stdlib/std/. editors/vscode/bin/lib/std/
    cd editors/vscode && npm install
    cd editors/vscode && npm run compile

package-vscode-ext: vscode-ext
    cd editors/vscode && npx @vscode/vsce package --allow-missing-repository --skip-license

install: make
    mkdir -p "${GOML_HOME:-$HOME/.goml}/bin"
    cp bin/stage2/gomlc "${GOML_HOME:-$HOME/.goml}/bin/gomlc"
    cp bin/stage2/goml "${GOML_HOME:-$HOME/.goml}/bin/goml"
    cp bin/stage2/gomlfmt "${GOML_HOME:-$HOME/.goml}/bin/gomlfmt"
    cp bin/stage2/gomllsp "${GOML_HOME:-$HOME/.goml}/bin/gomllsp"
    mkdir -p "${GOML_HOME:-$HOME/.goml}/lib/std"
    cp -R stdlib/std/. "${GOML_HOME:-$HOME/.goml}/lib/std/"
    cp stdlib/builtin_prelude.gom "${GOML_HOME:-$HOME/.goml}/lib/builtin_prelude.gom"

generate-stdlib-source:
    bash tools/stdlib/generate_source.sh

update-golden: make
    cd gomlc && UPDATE_EXPECT=1 ../bin/stage2/goml test formatter --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    cd gomlc && UPDATE_EXPECT=1 ../bin/stage2/goml test pipeline_test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    cd gomlc && UPDATE_EXPECT=1 GOML_TEST_GOML=../bin/stage2/goml GOML_TEST_GOMLC=../bin/stage2/gomlc ../bin/stage2/goml test compiler_test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m

verify-golden: make
    cd gomlc && ../bin/stage2/goml test formatter --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    cd gomlc && ../bin/stage2/goml test pipeline_test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
    cd gomlc && GOML_TEST_GOML=../bin/stage2/goml GOML_TEST_GOMLC=../bin/stage2/gomlc ../bin/stage2/goml test compiler_test --compiler ../bin/stage2/gomlc --jobs 16 --timeout 10m
