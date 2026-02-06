# Repository Guidelines

The project you are currently working on is called goml.

goml is a statically typed language with syntax similar to Rust, but it includes garbage collection (GC) and compiles to Go, so it does not require lifetime annotations and has no ownership system. In essence, it is closer in nature to OCaml or ML (Meta Language).

In goml, top-level functions must have fully explicit type signatures, and only top-level functions support generic type parameters. Generics are denoted using square brackets. Closures can be defined within functions, but they must have a single concrete type — goml does not include let-polymorphism (a.k.a. let-generalization). goml also supports defining traits, as well as enums and structs similar to Rust, and allows user-defined traits.

The generated Go code does not include generics — goml performs monomorphization by instantiating its own generic function calls. The generated code also does not contain Go closures — goml applies lambda lifting by performing lambda lifting (via lambda lifting) on its local functions.

The file extension for goml source files is `.gom`.

## Project Snapshot
- Go/Rust-inspired language frontend lowers through `lexer → parser → CST → AST → HIR → typed AST → Core -> Mono -> Lift → ANF` before emitting Go (`crates/compiler/src/go`).
- The CLI driver in `crates/compiler/src/main.rs` prints generated Go; regression tests in `crates/compiler/src/tests` compare every IR stage and execute the Go output.
- The `webapp` folder hosts a Vite/React playground using Wasm bindings from `crates/wasm-app`; it can display each IR stage while execution is stubbed out.
- `typer/name_resolution.rs` should only handle AST → HIR lowering plus pure name/visibility resolution; avoid making decisions that depend on `GlobalTypeEnv` or type information.
- `typer/check.rs` should only handle HIR → TAST type inference, checking, and constraint generation; avoid name-resolution responsibilities such as "fallback resolution paths" or cross-package name lookup.
- Any "ambiguity" should produce recoverable diagnostics; never `panic!` inside env/lookup. When multiple candidates exist, return `None` and report the error at a higher level.


## Project Structure & Module Organization
- Rust workspace in `crates/*`:
  - `lexer`, `parser`, `cst`, `ast`, `compiler` (core pipeline and tests), `wasm-app` (Rust → Wasm bindings), `lsp-server` (Language Server Protocol).
- Frontend in `webapp` (Vite + React + TypeScript) consuming `crates/wasm-app/pkg`.
- VS Code extension in `editors/vscode/` consuming LSP server binary.
- CI/dev helpers in `.justfile`. Build artifacts in `target/` and `webapp/dist/`.

## Project Configuration (`goml.toml`)

GoML projects use a `goml.toml` file for project configuration, similar to Cargo.toml in Rust.

A **module** is the top-level compilation unit containing one or more packages. The module root has a `goml.toml` with a `[module]` section. Sub-packages can have their own `goml.toml` files (without `[module]`) or rely on directory-based discovery.

### Module Root Example
```toml
[module]
name = "myapp"

[package]
name = "main"
entry = "main.gom"
```

### Sub-Package Example
```toml
[package]
name = "utils"
```

### Minimal Example (no module section, backward compatible)
```toml
[package]
name = "Main"
```

### Fields
| Field           | Required | Default     | Description                                  |
| --------------- | -------- | ----------- | -------------------------------------------- |
| `module.name`   | No       | -           | Module name (presence indicates module root) |
| `package.name`  | Yes      | -           | Package name (any valid identifier)          |
| `package.entry` | No       | `"lib.gom"` | Path to entry file relative to goml.toml     |

### Project Discovery
- The LSP and CLI search upward from the current file to find `goml.toml` with a `[module]` section
- If found, uses `package.entry` as the compilation entry point
- Sub-packages are discovered from subdirectories when imported, using their own `goml.toml` if present
- Package names can be any valid identifier (lowercase or PascalCase)

## VS Code LSP Extension

### Design Philosophy
- Reuse existing compiler infrastructure: the LSP server delegates to `crates/compiler/src/query.rs` which already powers the Monaco web editor.
- Full compilation on every request: no incremental/salsa-based caching yet; each hover/completion triggers a full typecheck of the entry package and its dependencies.
- Multi-package aware: the LSP discovers packages via `pipeline::packages::discover_packages` starting from the module root, supporting arbitrary package names + subdirectory library packages.

### Build Commands
- `just build-lsp`: Build release LSP binary (`target/release/goml-lsp`).
- `just install-lsp`: Build and copy binary to `editors/vscode/bin/`.
- `just vscode-ext`: Full build (LSP + extension TypeScript).

### Development Workflow
1. Build LSP: `cargo build -p lsp-server --release`
2. Copy binary: `cp target/release/goml-lsp editors/vscode/bin/`
3. Install deps: `cd editors/vscode && pnpm install`
4. Compile TS: `pnpm run compile`
5. Press F5 in VS Code to launch Extension Development Host.

### Configuration
- `goml.serverPath`: Custom path to `goml-lsp` binary (defaults to bundled or PATH lookup).
- `goml.trace.server`: Trace LSP communication (`off`, `messages`, `verbose`).

## Build, Test, and Development Commands
- Rust build: `cargo build` (workspace). Specific crate: `cargo build -p parser`.
- Rust tests: `cargo test`
- CLI: run goml programs via `cargo run -- run <file.gom>`; add `--dump-ast|--dump-hir|--dump-tast|--dump-core|--dump-mono|--dump-lift|--dump-anf|--dump-go` to print IR stages before execution.
- Lint (Rust): `just clippy` (equivalent to `cargo clippy --all-targets --all-features --locked -- -D warnings`).
- Format (Rust): `cargo fmt`.
- Wasm build: `wasm-pack build ./crates/wasm-app`.
- Webapp dev: `just start` (build Wasm, then `pnpm install` and `pnpm run dev` in `webapp`).
- Webapp build/preview: `pnpm -C webapp build` and `pnpm -C webapp preview`.
- CI locally: cargo check, test, fmt, clippy.

## Coding Style & Naming Conventions
- do not write any comments, instead, write clear and self-explanatory code
- Rust (edition 2024): format with `rustfmt`; deny clippy warnings. Use snake_case for functions/modules, CamelCase for types, SCREAMING_SNAKE_CASE for consts.
- TypeScript/React: 2‑space indent; PascalCase components; named exports preferred. Lint with `pnpm -C webapp lint`.
- Keep modules small and focused; avoid cross‑crate leakage—public APIs live in each crate’s `lib.rs`.

## Testing Guidelines
- We use expect-test, there are two basic commands:
  - `cargo test` run test to match golden snapshots
  - `env UPDATE_EXPECT=1 cargo test` to update snapshots
- Aim for fast, deterministic tests; cover parsing/typing edges with minimal fixtures when relevant.

### Single-File Pipeline Tests
- Each pipeline test case is in its own directory under `crates/compiler/src/tests/pipeline/`. Directory names follow the pattern `NNN/` (e.g., `000/`, `001/`) or `NNN_description/` (e.g., `007_expr_pattern_matching/`, `025_missing_match/`).
- Each test directory contains:
  - `main.gom` - the input source file
  - `main.gom.cst`, `main.gom.ast`, `main.gom.hir`, `main.gom.tast`, `main.gom.core`, `main.gom.mono`, `main.gom.anf`, `main.gom.go` - expected IR outputs at each compilation stage
  - `main.gom.out` - expected execution output
- You can quick check a test case with: `cargo run -- crates/compiler/src/tests/pipeline/001/main.gom`
- You should NEVER manually modify the generated files (`.cst`, `.ast`, `.hir`, `.tast`, `.core`, `.mono`, `.anf`, `.go`, `.out`). The only way to update them is by running: `env UPDATE_EXPECT=1 cargo test`.
- When asked to "add pipeline tests", create a new directory (e.g., `063/` or `063_feature_name/`) under `crates/compiler/src/tests/pipeline/` with a `main.gom` file, then run `env UPDATE_EXPECT=1 cargo test` to generate the expected outputs.

### Multi-Module Tests
- Multi-module tests are located in `crates/compiler/src/tests/module/`. They test the compiler's ability to handle multiple packages within a module.
- Each project directory follows the pattern `projectNNN/` (e.g., `project001/`, `project002/`) or `projectNNN_description/`.
- Structure of a multi-module project:
  - `goml.toml` - module configuration with `[module]` and `[package]` sections
  - `main.gom` - the entry point with `package Main;` declaration
  - `PackageName/goml.toml` - sub-package configuration with `[package]` section
  - `PackageName/lib.gom` - library package files, each with its own `package PackageName;` declaration
  - `main.gom.out` - expected execution output
- Package naming conventions:
  - The main package is typically declared as `package Main;` and contains the `fn main()` entry point
  - Library packages use PascalCase names (e.g., `package Lib;`, `package Util;`, `package Math;`)
  - Directory names must match the package names (e.g., `Lib/lib.gom` contains `package Lib;`)
- Packages can import other packages using `use PackageName;` syntax
- Inter-package dependencies:
  - Packages can import and use types, functions, and enums from other packages
  - Access package members using `PackageName::member` syntax (e.g., `Lib::Color::Red`, `Math::Pair`)
  - Trait method syntax `x.method(...)` for non-`dyn` values is enabled by `use PackageName::Trait` (builtin traits like `Show` are in the prelude and do not require `use`)
- To add a new multi-module test:
  1. Create a new directory under `crates/compiler/src/tests/module/` (e.g., `project011/`)
  2. Create `goml.toml` with `[module]` and `[package]` sections
  3. Create `main.gom` with `package Main;` and `fn main()`
  4. Create subdirectories for each library package with their `goml.toml` and `lib.gom` files
  5. Run `env UPDATE_EXPECT=1 cargo test` to generate the expected output file
- Note: Multi-module tests only generate `.out` files, not intermediate IR stages like pipeline tests

## Commit & Pull Request Guidelines
- Prefer Conventional Commits (`feat:`, `fix:`, `refactor:`, `chore:`). Be concise and imperative: "add parser error for ...".
- PRs: include a clear description, linked issues, and before/after notes or screenshots for web UI changes.
- Required: run cargo check, test, fmt, clippy locally; ensure no clippy or fmt diffs.
- Always run tests before notifying the user that a task is complete.

## Environment & Tooling
- Requirements: Rust toolchain, `wasm-pack`, Node 18+, `pnpm`.
- If adding crates, update workspace in `Cargo.toml`; keep inter‑crate deps via `[workspace.dependencies]`.

## GoML Language Specification

For the complete GoML language specification, see [SPEC.md](SPEC.md).

Key language characteristics:
* Statically typed with syntax similar to Rust but with garbage collection
* Top-level functions support generic type parameters (denoted with square brackets)
* Closures have a single concrete type (no let-polymorphism)
* Compiles to Go via monomorphization and lambda lifting
* File extension: `.gom`

For testing and development notes specific to the compiler implementation, see the Testing Guidelines section above.
