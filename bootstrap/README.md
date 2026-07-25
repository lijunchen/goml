# gomlc bootstrap

This directory contains the self-hosted GoML compiler. It is a GoML module named `gomlc` that mirrors the Rust compiler and implements the compiler driver protocol used by the `goml` CLI.

The compiler covers the complete single-file pipeline:

```text
lexer → parser → CST → AST → HIR → TAST → Core → Mono → Lift → ANF → Go
```

It also supports package checking, package builds, test builds, linking, execution, compiler artifacts, the standard library, and runtime host hooks.

## Build

Build the Rust driver and then compile the bootstrap module from the repository root:

```sh
cargo build -p goml -p gomlc
target/debug/goml build bootstrap/cmd/gomlc
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

Use the bootstrap compiler with the Rust `goml` project driver:

```sh
target/debug/goml check --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc path/to/project
target/debug/goml build --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc path/to/project
target/debug/goml run --compiler bootstrap/_artifact/bin/cmd/gomlc/gomlc path/to/project
```

The bootstrap binary also exposes the driver-facing `check`, `test-check`, `build`, `test-build`, `link`, and `test-link` commands.

## Bootstrap package tests

Run the GoML package tests with the Rust driver:

```sh
target/debug/goml test bootstrap/cmd/gomlc
target/debug/goml test bootstrap/lexer
target/debug/goml test bootstrap/parser
target/debug/goml test bootstrap/ast
target/debug/goml test bootstrap/hir
target/debug/goml test bootstrap/tast
target/debug/goml test bootstrap/core
target/debug/goml test bootstrap/mono
target/debug/goml test bootstrap/lift
target/debug/goml test bootstrap/anf
target/debug/goml test bootstrap/go_backend
target/debug/goml test bootstrap/stdlib
```

## Differential tests

The Rust differential tests live in `crates/compiler/src/tests/bootstrap`. They build the bootstrap compiler and compare it byte for byte with the Rust compiler across generated inputs, the repository corpus, compiler test suites, and pipeline snapshots.

The GoML pipeline snapshot tests cover every non-empty fixture under `crates/compiler/src/tests/pipeline`:

```sh
just test-bootstrap-pipeline
```

Run only the bootstrap differential tests:

```sh
cargo test -p compiler --features bootstrap-tests tests::bootstrap:: -- --test-threads=1
```

The tests are disabled by default and are not included in the normal workspace `cargo test`.

`GOML_REPO`, `GOML_BIN`, `RUST_GOMLC_BIN`, and `BOOTSTRAP_GOMLC_BIN` can override the repository and compiler paths. Set `BOOTSTRAP_GOMLC_SKIP_BUILD=1` to use an existing bootstrap binary.
