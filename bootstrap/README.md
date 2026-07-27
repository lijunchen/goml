# gomlc bootstrap

This directory contains the self-hosted GoML compiler. It is a GoML module named `gomlc` that mirrors the Rust compiler and implements the compiler driver protocol used by the `goml` CLI.

The compiler covers the complete single-file pipeline:

```text
lexer → parser → CST → AST → HIR → TAST → Core → Mono → Lift → ANF → Go
```

It also supports package checking, package builds, test builds, linking, execution, compiler artifacts, the standard library, and runtime host hooks.

## Build

Build the Rust driver and then compile the complete bootstrap module:

```sh
cargo build -p goml -p gomlc
cd bootstrap
../target/debug/goml build --compiler ../target/debug/gomlc
```

The resulting compiler is:

```text
bootstrap/_artifact/bin/cmd/gomlc/gomlc
```

## Single-file commands

Run a GoML source file:

```sh
bootstrap/_artifact/bin/cmd/gomlc/gomlc run-single path/to/file.gom
```

Dump selected compiler stages before execution:

```sh
bootstrap/_artifact/bin/cmd/gomlc/gomlc run-single \
  --dump-ast \
  --dump-hir \
  --dump-tast \
  --dump-core \
  --dump-mono \
  --dump-lift \
  --dump-anf \
  --dump-go \
  path/to/file.gom
```

Print one stage directly:

```sh
bootstrap/_artifact/bin/cmd/gomlc/gomlc lex path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc cst path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc ast path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc hir path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc tast path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc core path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc mono path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc lift path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc anf path/to/file.gom
bootstrap/_artifact/bin/cmd/gomlc/gomlc go path/to/file.gom
```

## Project commands

Run whole-project commands from anywhere inside the target module:

```sh
cd path/to/project
/path/to/goml/target/debug/goml check --compiler /path/to/goml/bootstrap/_artifact/bin/cmd/gomlc/gomlc
/path/to/goml/target/debug/goml build --compiler /path/to/goml/bootstrap/_artifact/bin/cmd/gomlc/gomlc
/path/to/goml/target/debug/goml run --compiler /path/to/goml/bootstrap/_artifact/bin/cmd/gomlc/gomlc
```

The bootstrap binary also exposes the driver-facing `check`, `test-check`, `build`, `test-build`, `link`, and `test-link` commands.

## Bootstrap package tests

Run all GoML package tests with the Rust driver:

```sh
cd bootstrap
../target/debug/goml test --compiler ../target/debug/gomlc
```

The first positional argument is a test-name filter:

```sh
../target/debug/goml test lexer --compiler ../target/debug/gomlc
../target/debug/goml test parser --compiler ../target/debug/gomlc
```

## Differential tests

The Rust differential tests live in `crates/compiler/src/tests/bootstrap`. They build the bootstrap compiler and compare it byte for byte with the Rust compiler across generated inputs, the repository corpus, compiler test suites, and pipeline snapshots.

The GoML pipeline snapshot tests cover every non-empty fixture under `crates/compiler/src/tests/pipeline`:

```sh
just test-bootstrap-pipeline
```

The GoML compiler corpus tests cover e2e programs, diagnostics, module projects, and crashers:

```sh
just test-bootstrap-compiler
```

Run only the bootstrap differential tests:

```sh
cargo test -p compiler --features bootstrap-tests tests::bootstrap:: -- --test-threads=1
```

The tests are disabled by default and are not included in the normal workspace `cargo test`.

`GOML_REPO`, `GOML_BIN`, `RUST_GOMLC_BIN`, and `BOOTSTRAP_GOMLC_BIN` can override the repository and compiler paths. Set `BOOTSTRAP_GOMLC_SKIP_BUILD=1` to use an existing bootstrap binary.
