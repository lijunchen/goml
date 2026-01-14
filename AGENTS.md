# Repository Guidelines

The project you are currently working on is called goml.

goml is a statically typed language with syntax similar to Rust, but it includes garbage collection (GC) and compiles to Go, so it does not require lifetime annotations and has no ownership system. In essence, it is closer in nature to OCaml or ML (Meta Language).

In goml, top-level functions must have fully explicit type signatures, and only top-level functions support generic type parameters. Generics are denoted using square brackets. Closures can be defined within functions, but they must have a single concrete type — goml does not include let-polymorphism (a.k.a. let-generalization). goml also supports defining traits, as well as enums and structs similar to Rust, and allows user-defined traits.

The generated Go code does not include generics — goml performs monomorphization by instantiating its own generic function calls. The generated code also does not contain Go closures — goml applies lambda lifting by performing lambda lifting (via lambda lifting) on its local functions.

The file extension for goml source files is `.gom`.

## Project Snapshot
- Go/Rust-inspired language frontend lowers through `lexer → parser → CST → AST → FIR → typed AST → Core -> Mono -> Lift → ANF` before emitting Go (`crates/compiler/src/go`).
- The CLI driver in `crates/compiler/src/main.rs` prints generated Go; regression tests in `crates/compiler/src/tests` compare every IR stage and execute the Go output.
- The `webapp` folder hosts a Vite/React playground using Wasm bindings from `crates/wasm-app`; it can display each IR stage while execution is stubbed out.

## Project Structure & Module Organization
- Rust workspace in `crates/*`:
  - `lexer`, `parser`, `cst`, `ast`, `compiler` (core pipeline and tests), `wasm-app` (Rust → Wasm bindings).
- Frontend in `webapp` (Vite + React + TypeScript) consuming `crates/wasm-app/pkg`.
- CI/dev helpers in `.justfile`. Build artifacts in `target/` and `webapp/dist/`.

## Build, Test, and Development Commands
- Rust build: `cargo build` (workspace). Specific crate: `cargo build -p parser`.
- Rust tests: `cargo test`
- CLI: run goml programs via `cargo run -- run <file.gom>`; add `--dump-ast|--dump-fir|--dump-tast|--dump-core|--dump-mono|--dump-lift|--dump-anf|--dump-go` to print IR stages before execution.
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
  - `main.gom.cst`, `main.gom.ast`, `main.gom.fir`, `main.gom.tast`, `main.gom.core`, `main.gom.mono`, `main.gom.anf`, `main.gom.go` - expected IR outputs at each compilation stage
  - `main.gom.out` - expected execution output
- You can quick check a test case with: `cargo run -- crates/compiler/src/tests/pipeline/001/main.gom`
- You should NEVER manually modify the generated files (`.cst`, `.ast`, `.fir`, `.tast`, `.core`, `.mono`, `.anf`, `.go`, `.out`). The only way to update them is by running: `env UPDATE_EXPECT=1 cargo test`.
- When asked to "add pipeline tests", create a new directory (e.g., `063/` or `063_feature_name/`) under `crates/compiler/src/tests/pipeline/` with a `main.gom` file, then run `env UPDATE_EXPECT=1 cargo test` to generate the expected outputs.

### Multi-Package Tests
- Multi-package tests are located in `crates/compiler/src/tests/package/`. They test the compiler's ability to handle multiple packages and inter-package dependencies.
- Each project directory follows the pattern `projectNNN/` (e.g., `project001/`, `project002/`) or `projectNNN_description/`.
- Structure of a multi-package project:
  - `main.gom` - the entry point with `package Main` declaration
  - `PackageName/lib.gom` - library package files, each with its own `package PackageName` declaration
  - `main.gom.out` - expected execution output
- Package naming conventions:
  - The main package must be declared as `package Main` and contain the `fn main()` entry point
  - Library packages use PascalCase names (e.g., `package Lib`, `package Util`, `package Math`)
  - Directory names must match the package names (e.g., `Lib/lib.gom` contains `package Lib`)
- Packages can import other packages using `import PackageName` syntax
- Inter-package dependencies:
  - Packages can import and use types, functions, and enums from other packages
  - Access package members using `PackageName::member` syntax (e.g., `Lib::Color::Red`, `Math::Pair`)
- To add a new multi-package test:
  1. Create a new directory under `crates/compiler/src/tests/package/` (e.g., `project006/`)
  2. Create `main.gom` with `package Main` and `fn main()`
  3. Create subdirectories for each library package with their `lib.gom` files
  4. Run `env UPDATE_EXPECT=1 cargo test` to generate the expected output file
- Note: Multi-package tests only generate `.out` files, not intermediate IR stages like pipeline tests

## Commit & Pull Request Guidelines
- Prefer Conventional Commits (`feat:`, `fix:`, `refactor:`, `chore:`). Be concise and imperative: “add parser error for …”.
- PRs: include a clear description, linked issues, and before/after notes or screenshots for web UI changes.
- Required: run cargo check, test, fmt, clippy locally; ensure no clippy or fmt diffs.
- Always run tests before notifying the user that a task is complete.

## Environment & Tooling
- Requirements: Rust toolchain, `wasm-pack`, Node 18+, `pnpm`.
- If adding crates, update workspace in `Cargo.toml`; keep inter‑crate deps via `[workspace.dependencies]`.
