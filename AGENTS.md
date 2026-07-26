# Repository Guidelines

The project you are currently working on is called goml.

goml is a statically typed language with syntax similar to Rust, but it includes garbage collection (GC) and compiles to Go, so it does not require lifetime annotations and has no ownership system. In essence, it is closer in nature to OCaml or ML (Meta Language).

In goml, top-level functions must have fully explicit type signatures, and only top-level functions support generic type parameters. Generics are denoted using square brackets. Closures can be defined within functions, but they must have a single concrete type — goml does not include let-polymorphism (a.k.a. let-generalization). goml also supports defining traits, as well as enums and structs similar to Rust, and allows user-defined traits.

The generated Go code does not include generics — goml performs monomorphization by instantiating its own generic function calls. The generated code also does not contain Go closures — goml applies lambda lifting by performing lambda lifting (via lambda lifting) on its local functions.

The file extension for goml source files is `.gom`.

## Project Snapshot
- Go/Rust-inspired language frontend lowers through `lexer → parser → CST → AST → HIR → typed AST → Core -> Mono -> Lift → ANF` before emitting Go (`crates/compiler/src/go`).
- The `goml` CLI in `crates/goml` handles project builds and package management, while `gomlc` in `crates/gomlc` exposes compiler commands; regression tests in `crates/compiler/src/tests` compare every IR stage and execute the Go output.
- The `webapp` folder hosts a Vite/React playground using Wasm bindings from `crates/wasm-app`; it can display each IR stage while execution is stubbed out.
- `typer/name_resolution.rs` should only handle AST → HIR lowering plus pure name resolution and visibility metadata; avoid making decisions that depend on `GlobalTypeEnv` or type information. Cross-package export filtering happens at package/interface construction time.
- `typer/check.rs` should only handle HIR → TAST type inference, checking, and constraint generation; avoid name-resolution responsibilities such as "fallback resolution paths" or cross-package name lookup.
- Any "ambiguity" should produce recoverable diagnostics; never `panic!` inside env/lookup. When multiple candidates exist, return `None` and report the error at a higher level.


## ANF IR and Join Points

The ANF (A-Normal Form) IR is the last intermediate representation before Go code generation. It lives in `crates/compiler/src/anf.rs` and is produced by the `anf_file` pass from the Lift IR.

### Core Data Types

```
Block { binds: Vec<Bind>, term: Term }
```
A block is a sequence of bindings followed by a terminal. This is the fundamental unit of ANF.

- **`Bind::Let(LetBind)`** — `let x = expr in ...` — binds a value expression to a local variable.
- **`Bind::Join(JoinBind)`** — `join k(params) { body } in ...` — declares a non-recursive join point (a local continuation).
- **`Bind::JoinRec(Vec<JoinBind>)`** — `joinrec { join k(params) { body } } in ...` — declares mutually recursive join points (used for loops).

Terminal terms (`Term`):
- **`Return(imm)`** — return a value from the function.
- **`Jump { target, args }`** — jump to a join point, passing arguments.
- **`If { cond, then_, else_ }`** — conditional branch; each branch is a `Block`.
- **`Match { scrut, arms, default }`** — pattern match; each arm body is a `Block`.
- **`Unreachable`** — marks dead code.

### What Are Join Points?

Join points are local continuations that represent "where control flow merges." They are the ANF equivalent of phi nodes in SSA, but are more structured — they have parameters (like function arguments) and a body.

**Key insight**: join points are NOT functions. They cannot escape their scope, are always called in tail position via `Jump`, and are guaranteed to be called exactly in the lexical scope where they are defined.

### How GoML Source Maps to ANF Join Points

**if-else expressions** (when used as expressions or with code after them):
```
// GoML source:
let result = if cond { a } else { b };
use(result)

// ANF:
join k(result) { use(result) } in    // k is the continuation after the if
if cond {
    jump k(a)                         // both branches jump to k
} else {
    jump k(b)
}
```

**Nested if-else** chains produce nested joins:
```
// GoML source:
let r = if c1 { "a" } else if c2 { "b" } else { "c" };

// ANF:
join k_outer(r) { ... } in
if c1 {
    jump k_outer("a")
} else {
    join k_inner(tmp) { jump k_outer(tmp) } in
    if c2 { jump k_inner("b") } else { jump k_inner("c") }
}
```

**match expressions** produce similar patterns — all arms jump to the same continuation join.

**while loops** lower to `JoinRec`:
```
// GoML source:
while i.get() < limit {
    body;
}

// ANF:
join exit() { ... } in
joinrec {
    join loop() {
        let cond = i.get() < limit in
        if cond {
            <body binds>
            jump loop()           // continue → self-jump
        } else {
            jump exit()           // break → jump to exit
        }
    }
} in
jump loop()
```

### Go Code Generation from ANF (`crates/compiler/src/go/compile.rs`)

The Go emitter produces structured code (if/else, switch, for) — never goto/label. It works by recognizing patterns in the ANF:

**Pattern 1 — Continuation join**: When all branches of an `If` or `Match` jump to the same join `k`, the emitter outputs the if/switch with only argument assignments in each branch, then emits `k`'s body AFTER the if/switch as straight-line code. This is the most common pattern.

**Pattern 2 — While loop**: When a `JoinRec` has a single member whose body is `if cond { <body>; jump self() } else { jump exit() }`, it emits `for { if !cond { break }; <body> }`. The `terminates_at` helper checks whether a branch eventually reaches the self-jump (through arbitrary let-binds and nested joins), not just immediate self-jumps.

**Pattern 3 — Inline join at Jump**: When a `Jump` targets a join that is NOT a pending continuation (e.g., a join defined in an outer scope), its body is inlined at the jump site.

Key functions in the emitter:
- `compile_fn` — entry point; builds a flat `JoinEnv` of all joins, then calls `compile_block_structured`.
- `build_join_env` / `collect_joins_from_block` — recursively collects all non-while-loop joins into a flat lookup table. While-loop `JoinRec` members are excluded.
- `compile_block_structured` — walks `binds` sequentially, collects `pending_joins` from `Bind::Join`, compiles `Bind::JoinRec` as while loops, then dispatches the terminal.
- `compile_term_with_continuations` — checks if all branches target the same pending join (via `all_branches_jump_to`); if so, emits the branching construct with arg assignments, then emits the join body.
- `compile_while_loop` — emits `for { if !cond { break }; body }`. Adds the loop's `JoinId` to `continue_targets` in `JoinEnv` so that self-jumps inside the body are compiled as implicit continue (empty statement at end of loop iteration).
- `compile_term_leaf` — fallback for terms that don't match a continuation pattern; inlines join bodies at `Jump` sites.

### Design Invariants

- The `JoinEnv` is a flat `HashMap<JoinId, JoinBind>` built once per function. While-loop `JoinRec` members are NOT in `JoinEnv` — they are compiled directly by `compile_while_loop`.
- `continue_targets: HashSet<JoinId>` tracks which `JoinId`s represent the current enclosing while loop. A `Jump` to a continue target emits nothing (the `for` loop naturally continues).
- Join bodies form a DAG (except `JoinRec` which is handled specially). The emitter inlines join bodies at their use sites, which is safe because each non-recursive join is used at most once in the continuation position and any remaining uses are in tail position.
- DCE (`crates/compiler/src/go/dce.rs`) runs on the Go AST after emission. Since the structured emitter produces no goto/label, DCE is always enabled.

## Project Structure & Module Organization
- Rust workspace in `crates/*`:
  - `lexer`, `parser`, `cst`, `ast`, `compiler` (core pipeline and tests), `gomlc` (compiler CLI), `goml-project` (manifest, registry, and package graph), `goml` (build and package-manager CLI), `wasm-app` (Rust → Wasm bindings), `lsp-server` (Language Server Protocol).
- Frontend in `webapp` (Vite + React + TypeScript) consuming `crates/wasm-app/pkg`.
- VS Code extension in `editors/vscode/` consuming LSP server binary.
- CI/dev helpers in `.justfile`. Build artifacts in `target/` and `webapp/dist/`.

## Project Configuration (`goml.toml`)

GoML projects use a `goml.toml` file for project configuration. A module owns a canonical path and contains one or more directory-based packages.

### Module Example
```toml
[module]
path = "alice::myapp"

[build]
target-dir = "_artifact"

[dependencies]
"alice::http" = "1.2.0"
```

### Package Example
```goml
package main;

use alice::myapp::utils;

fn main() -> unit {
    string_println(utils::message())
}
```

```text
utils/utils.gom
```
```goml
package utils;

pub fn message() -> string {
    "hello"
}
```

### Fields
| Field          | Required | Default | Description |
| -------------- | -------- | ------- | ----------- |
| `module.path`  | Yes      | -       | Canonical module path |
| `build.target-dir` | No   | `_artifact` | Module-relative root for GoML build artifacts |
| `dependencies` | No       | `{}`    | Third-party modules keyed by canonical path with minimum version `X.Y.Z` |

### Project Discovery
- The LSP and CLI search upward from the current file or target directory for `goml.toml` with a `[module]` section.
- Every directory containing `.gom` files is a package. All `.gom` files in that directory must declare the same `package name;`.
- A package's canonical identity is its directory path appended to `module.path`; its declared package name is the default local `use` alias.
- `use full::package::path;` loads a package into one source file. `use full::package::path as alias;` selects an explicit alias.
- `use alias::Trait;` brings a trait from an already loaded package into method-call scope.
- `mod`, `crate::`, `self::`, `super::`, and rooted `::path` syntax are unsupported.
- `goml check`, `goml build`, and `goml test` always operate on the complete module found from the current directory and do not accept package targets.
- `goml build` links every package declared as `package main`; each executable entry must define `fn main()`.
- A `tests` directory is one black-box test package for its parent package. Its direct `.gom` files declare `package tests;`; nested test-suite directories are unsupported.
- Third-party dependencies are declared only in the module-root `goml.toml`.
- Standalone single-file compiler commands may omit `package main;` and retain implicit builtin behavior.

### Current Visibility Semantics
- Cross-package access is filtered by top-level item visibility. A package can use another package's `pub fn`, `pub struct`, `pub enum`, and `pub trait`; private top-level items are not exported.
- Private helpers remain usable in every file of their defining package, including from public wrapper functions.
- Struct fields and inherent `impl` methods are private by default and require `pub` for cross-package access. Trait implementation methods inherit the trait method visibility and must not use `pub`.
- Interface files and external dependency artifacts export only public top-level API. Each package has its own interface and Core artifact.

## Package Management

GoML currently uses a mono-repo registry model for third-party dependencies.

### Registry Layout
- The registry is a git repository with an `index.toml` at the repo root
- Each published module version lives at `/<owner>/<module>/<version>/`
- Each published module version must include its own `goml.toml`
- The module root inside the registry must declare `[module] path = "<owner>::<module>"`
- `index.toml` is the authoritative package index; package management reads the index instead of scanning the full registry tree

### Dependency Semantics
- Dependencies are declared in `[dependencies]` using `"owner::module" = "X.Y.Z"`
- Versions use strict semver `X.Y.Z` only; prerelease and build metadata are not supported
- Version values are minimum version requirements, not exact pins
- Resolution uses Go-style MVS: the selected version for each module is the minimum available version satisfying the highest required minimum in the graph
- Registry entries are treated as immutable after publication
- There is currently no `goml.lock`

### User Config And Cache
- Global GoML state lives under `~/.goml`
- `GOML_HOME` overrides the default `~/.goml` location
- `~/.goml/config.toml` stores the default registry URL under `[registry].default`
- `~/.goml/cache/registry` stores the cloned mono-repo registry cache
- `goml update` creates `~/.goml/config.toml` if it does not exist yet

### CLI Commands
- `goml update` clones or fast-forwards the registry cache into `~/.goml/cache/registry`
- `goml add owner::module` adds the latest indexed version requirement to the current module-root `goml.toml`
- `goml add owner::module@X.Y.Z` adds or updates an explicit minimum version requirement
- `goml remove owner::module` removes that dependency from the current module-root `goml.toml`
- `goml update`, `goml add`, and `goml remove` all support `--local-registry <path>` to use a local git repo or registry checkout instead of the default configured registry
- `goml add` and `goml remove` locate the module root by searching upward for `goml.toml` with a `[module]` section

### External Package Namespaces
- Registry source layout reuses normal directory-package layout.
- External packages are loaded by canonical path, for example `use alice::http;` or `use alice::http::client;`.
- The declared package name supplies the default local alias; `as` allows another alias.
- Trait scope uses the package alias, for example `use alice::http; use http::Show;`.
- Canonical package identities remain distinct, so packages with the same declared name from different owners can coexist when explicit aliases are used.

### Build, Check, And LSP Behavior
- `goml check`, `goml build`, compiler queries, and LSP requests all resolve and typecheck third-party dependencies
- Third-party source files remain in the registry cache under `~/.goml/cache/registry`
- Project outputs are written under `[build].target-dir`, which defaults to `_artifact` and can be overridden with `--target-dir`.
- Root-package executables are written to `<target-dir>/bin/<module-name>`. Nested entry packages preserve their module-relative package path and add the executable name, for example package `alice::app::cmd::server` is written to `<target-dir>/bin/cmd/server/server`.
- Local artifacts are materialized under `<target-dir>/{check|build}/pkg/<canonical-package-path>/<package-name>.*`.
- External dependency artifacts are materialized under `<target-dir>/{check|build}/deps/<owner>/<module>/<version>/pkg/<canonical-package-path>/<package-name>.*`.
- Linked Go sources are written beside the entry package Core artifact as `goml_generated.go`.
- Interfaces and dependency environments expose only public top-level API; current-package codegen still uses the package's full internal environment so private helpers compile normally.
- Go-to-definition, hover, completion, and other query features can resolve into cached third-party source files

## VS Code LSP Extension

### Design Philosophy
- Reuse existing compiler infrastructure: the LSP server delegates to `crates/compiler/src/query.rs` which already powers the Monaco web editor.
- Full compilation on every request: no incremental/salsa-based caching yet; each hover/completion triggers a full typecheck of the target package and its dependencies.
- Module/package aware: the LSP discovers directory packages from `[module]`, honors file-scoped aliases, and resolves definitions using canonical package identities.

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
- Rust tests: `cargo test` (bootstrap differential tests are disabled by default).
- Bootstrap differential tests: `cargo test -p compiler --features bootstrap-tests tests::bootstrap:: -- --test-threads=1`.
- CLI: use `cargo run -p goml -- new <project_name>` to scaffold a module with `main` and `lib` packages; run `goml check`, `goml build`, or `goml test` from anywhere inside a module for whole-project workflows; use `--dry-run` to print planned per-package compiler commands; use `cargo run -p gomlc -- run-single <file.gom>` for standalone execution; use `cargo run -p gomlc -- check|build ...` for per-package artifacts and `gomlc link --entry <canonical-package> ...` for explicit linking; add `--dump-ast|--dump-hir|--dump-tast|--dump-core|--dump-mono|--dump-lift|--dump-anf|--dump-go` to `gomlc run-single` to print IR stages before execution.
- `goml check` and `goml build` locate `gomlc` through `--compiler`, `GOMLC`, the directory containing `goml`, `GOML_HOME/bin`, then `PATH`; the driver protocol is verified before compilation.
- Lint (Rust): `just clippy` (equivalent to `cargo clippy --all-targets --all-features --locked -- -D warnings`).
- Format (Rust): `cargo fmt`.
- Wasm build: `wasm-pack build ./crates/wasm-app`.
- Webapp dev: `just start` (build Wasm, then `pnpm install` and `pnpm run dev` in `webapp`).
- Webapp build/preview: `pnpm -C webapp build` and `pnpm -C webapp preview`.
- CI locally: cargo check, test, fmt, clippy.

## Coding Style & Naming Conventions
- do not write any comments, instead, write simple, clear and self-explanatory code
- Rust (edition 2024): format with `rustfmt`; deny clippy warnings. Use snake_case for functions/modules, CamelCase for types, SCREAMING_SNAKE_CASE for consts.
- TypeScript/React: 2‑space indent; PascalCase components; named exports preferred. Lint with `pnpm -C webapp lint`.
- Keep packages small and focused. Public cross-package APIs must be explicitly marked `pub`; files in one directory form one package.

## Testing Guidelines
- We use expect-test, there are two basic commands:
  - `cargo test` run test to match golden snapshots
  - `env UPDATE_EXPECT=1 cargo test` to update snapshots
- Bootstrap differential tests require the non-default `bootstrap-tests` feature and are only run explicitly.
- Aim for fast, deterministic tests; cover parsing/typing edges with minimal fixtures when relevant.

### Single-File Pipeline Tests
- Each pipeline test case is in its own directory under `crates/compiler/src/tests/pipeline/`. Directory names follow the pattern `NNN/` (e.g., `000/`, `001/`) or `NNN_description/` (e.g., `007_expr_pattern_matching/`, `025_missing_match/`).
- Each test directory contains:
  - `main.gom` - the input source file
  - `main.gom.cst`, `main.gom.ast`, `main.gom.hir`, `main.gom.tast`, `main.gom.core`, `main.gom.mono`, `main.gom.anf`, `main.gom.go` - expected IR outputs at each compilation stage
  - `main.gom.out` - expected execution output
- You can quick check a test case with: `cargo run -p gomlc -- run-single crates/compiler/src/tests/pipeline/001/main.gom`
- You should NEVER manually modify the generated files (`.cst`, `.ast`, `.hir`, `.tast`, `.core`, `.mono`, `.anf`, `.go`, `.out`). The only way to update them is by running: `env UPDATE_EXPECT=1 cargo test`.
- When asked to "add pipeline tests", create a new directory (e.g., `063/` or `063_feature_name/`) under `crates/compiler/src/tests/pipeline/` with a `main.gom` file, then run `env UPDATE_EXPECT=1 cargo test` to generate the expected outputs.

### Multi-Package Tests
- Multi-package tests are located in `crates/compiler/src/tests/module/`. They test module manifests, directory packages, file-scoped imports, visibility, separate compilation, and linking.
- Each project directory follows the pattern `projectNNN/` (e.g., `project001/`, `project002/`) or `projectNNN_description/`.
- Structure of a multi-package project:
  - `goml.toml` - module configuration with `[module] path = "..."`
  - `main.gom` - root-directory package source, normally `package main;`
  - `name/name.gom` - child directory package source with `package name;`
  - `main.gom.out` - expected execution output
- Package conventions:
  - Every project source file declares its package.
  - The entry package contains `fn main()` and is declared `package main;`.
  - Every file explicitly imports the packages it references; imports are not shared across files in one package.
  - Cross-package APIs must be declared `pub`; private top-level items should only be used inside the defining package
  - Cross-package references use an imported alias, such as `math::Pair`.
  - Trait method syntax `x.method(...)` for non-`dyn` values is enabled by `use alias::Trait` after importing the package; builtin traits like `Show` are in the prelude.
- To add a new multi-package test:
  1. Create a new directory under `crates/compiler/src/tests/module/` (e.g., `project011/`)
  2. Create `goml.toml` with a `[module]` section
  3. Create the entry package file with `package main;`, imports, and `fn main()`
  4. Create child package files under their package directories
  5. Run `env UPDATE_EXPECT=1 cargo test` to generate the expected output file
- Note: Multi-package tests run package discovery and project execution, and only generate `.out` files, not intermediate IR stages like pipeline tests

## Commit & Pull Request Guidelines
- Prefer Conventional Commits (`feat:`, `fix:`, `refactor:`, `chore:`). Be concise and imperative: "add parser error for ...".
- PRs: include a clear description, linked issues, and before/after notes or screenshots for web UI changes.
- Required: run cargo check, test, fmt, clippy locally; ensure no clippy or fmt diffs.
- If a change updates expect snapshots, use two commits: first the logic change, then a snapshot-only commit generated via `env UPDATE_EXPECT=1 cargo test` (e.g. `chore(tests): update snapshots`). If the change also adds new tests, use three commits: (1) logic, (2) the new test source files, (3) snapshots.
- Always run tests before notifying the user that a task is complete.

## Environment & Tooling
- Requirements: Rust toolchain, `wasm-pack`, Node 18+, `pnpm`.
- If adding crates, update workspace in `Cargo.toml`; keep inter‑crate deps via `[workspace.dependencies]`.

## GoML Introduction

### Lexical Structure and Literals

* Primitive types: `bool`, `unit`/`()`, `int`, `int8/16/32/64`, `uint`, `uint8/16/32/64`, `float32/float64`, `string`, `char`.
* Literals: boolean, integer/unsigned/floating-point, string, and char literals.
  * Numeric literals have no type suffixes. Their type comes from context; unconstrained integer literals default to `int` and unconstrained floating-point literals default to `float64`.
  * Numeric literals may use `_` between digits. Floating-point literals support `e`/`E` scientific notation with an optional exponent sign.
  * Char literals use single quotes, e.g. `'a'`, and support escapes like `'\n'` and `'\u0041'`.
  * `char` represents a Unicode scalar value and compiles to Go `rune` (an `int32` code point).
* String concatenation with `+` is supported. Multiline strings continue lines with leading `\\` and may contain quotes and backslashes (see `062_multiline_string`).
* Tuples `(a, b, c)` and the wildcard `_` are commonly used in bindings and pattern matching.

### Bindings and Scope

* `let name = expr;` allows shadowing of the same name. Type annotations are supported, e.g. `let x: int32 = 1;`. `let _ = expr;` discards the result.
* `let` supports pattern destructuring: tuples, struct fields, enum constructors, and wildcards can be mixed.

### Functions and Closures

* Top-level function declaration: `fn name(params) -> Ret { ... }`. Top-level functions must have explicit signatures; if the return type is omitted, it defaults to `unit`.
* Only top-level functions may declare generic parameters, using square brackets, e.g. `fn id[T](x: T) -> T`.
* Top-level function generics may add trait bounds per parameter: `fn f[T: A + B + C](x: T) -> ...`. Associated types can be constrained with `where`, for example `fn drain[T, I: Iterator](iterator: I) -> unit where I::Item == T`. `A + B` is only valid in generic bounds (not in type annotations/param/return types, and not in `dyn`).
* Function types are written as `(A, B) -> C` and can be stored in arrays, passed as arguments, or returned.
* Closures are written as `|args| expr` or `|| { ... }`. They can capture outer variables, support multiple levels of nesting, and can return closures.

### Structs, Enums, and Fields

* Structs: `struct Name { field: Type, ... }`. Construction supports key–value syntax or field shorthand. Field access uses `value.field`, and fields can be used for update reconstruction.
* Enums: `enum Name { Variant, Variant(T1, T2), ... }`, with support for generics. Constructors may be uppercase or lowercase; namespaced access `Enum::Variant` avoids conflicts.
* Pattern matching supports field patterns, shorthand, and wildcards for structs; enum matching can destructure payloads or match constructors directly.

### Built-in Containers and References

* Fixed-length arrays `[T; N]`, literals `[1, 2, 3]`, accessed via `array_get/array_set`.
* Mutable references `Ref[T]`: created with `Ref::new(x)`, accessed with `x.get()`, and updated with `x.set(value)`; nested references are supported.
* Built-in growable vectors `Vec[T]`: `vec_new/vec_push/vec_get/vec_len`; `v.iter()` creates a `FnIterator[T]` implementing `Iterator` with `Item = T`.
* Built-in read-only slices `Slice[T]`: `slice/slice_get/slice_len/slice_sub`.
  * `slice(vec, start, end)` creates a bounded view over `Vec[T]`.
  * `Slice[T]` methods: `get`, `len`, `sub`, `iter`.
  * Current model is read-only view type; no `slice_set`/`push` on `Slice[T]`.
* Single-pass iterators implement `Iterator` and bind its associated `Item` type.
  * `FnIterator::from_fn(|| Option[T])` creates a closure-backed iterator and `iterator.next()` advances it.
  * `range(start, end)` creates a half-open `FnIterator[int]` over increasing values.
* Built-in `string` supports method syntax for common operations.
  * Prefer `s.len()` and `s.get(i)` over `string_len(s)` and `string_get(s, i)` in tests and examples.
  * Prefer method syntax such as `x.to_string()` when a builtin type or in-scope trait already exposes it.

### Control Flow and Expressions

* `if ... else ...` is an expression; branches may be nested. `while cond { ... }` loops return `unit`.
* `for pattern in source { ... }` accepts any value implementing `IntoIterator`, evaluates the source expression and `into_iter` conversion once, and returns `unit`. The pattern must be irrefutable and the body must return `unit`; `Vec[T]`, `Slice[T]`, and iterator values can be used directly.
* Boolean and arithmetic operators: `+ - * /`, unary negation, logical `! && ||`, and comparisons `== != < > <= >=`.
* `match expr { pattern => expr, ... }`: patterns are tried in order. Patterns include literals, tuples, structs, enums, bindings, and the wildcard `_`. Missing coverage results in an error (e.g., unmatched destructuring).
* `expr?` propagates `None` or `Err` from `Option[T]` and `Result[T, E]`. Other enum shapes do not opt into `?`; there is no user-defined `Try` trait yet.

### Traits and `impl`

* `trait T { fn method(Self, ...) -> ...; }` defines an interface. Traits may declare type parameters, for example `trait Convert[T] { fn convert(Self) -> T; }`.
* Implementations use `impl Trait for Type { ... }`; generic applications are part of impl identity, so one concrete type may implement both `Convert[int32]` and `Convert[string]`.
* Inherent implementations `impl Type { ... }` provide associated functions and methods.
* Invocation styles: method syntax `value.method(...)`, or associated syntax `Type::method(value, ...)` / `Trait::method(value, ...)`. Generic trait UFCS uses explicit arguments, such as `Convert::[int32]::convert(value)`.
  * For trait methods on non-`dyn` values, `x.method(...)` works when the trait is in scope via `use alias::Trait` after the defining package is imported (builtin traits like `Show` are in the prelude), otherwise use UFCS like `Trait::method(x, ...)`.
* When multiple trait bounds or applications provide the same method name, `x.foo()` is ambiguous and requires UFCS disambiguation (for example `A::foo(x)` or `Convert::[int32]::convert(x)`).
* Trait objects: `dyn Trait` is a first-class type for dynamic dispatch.
  * Coercion: when the expected type is `dyn Trait`, a value of concrete type `T` is implicitly converted if there is a visible `impl Trait for T`.
  * Calling: `Trait::method(x, ...)` works for both concrete `x: T` (static dispatch) and `x: dyn Trait` (dynamic dispatch).
  * Object safety (current): the receiver must be the first parameter and be exactly `Self`; `Self` is not allowed in other parameters or the return type.
  * Limitations (current): generic traits and traits with associated types are static-only and cannot be used as `dyn`; trait method call via `x.method(...)` is not supported for `dyn Trait` (use `Trait::method(x, ...)`); pattern matching on `dyn Trait` is not supported; `dyn TraitA + TraitB` and explicit `as dyn Trait` syntax are not implemented.

### Notes on `char`

* Codegen: goml `char` lowers to Go `rune`.
* Printing: `char` implements `ToString`; `c.to_string()` returns a `string` representation of the code point.
* Pattern matching: `match` supports `char` scrutinees and `char` literal patterns.

### Concurrency and Side Effects

* `go expr;` starts concurrent execution, commonly used with zero-argument closures.
* Common built-ins for I/O and debugging: `string_print/string_println`, and `_to_string` for basic types or derived `to_string`.

### Attributes and Derivation

* Attributes such as `#[derive(ToString)]`, `#[derive(Eq)]`, and `#[derive(Hash)]` automatically generate methods; applicable to structs and enums.

### External Interoperability

* User-defined Go FFI is not supported while the compiler is being bootstrapped.
* Only compiler-owned host hooks use `#[builtin] extern fn`; normal goml code should call those builtins instead of binding arbitrary Go symbols.

### Additional Conventions and Constraints

* Top-level functions must have explicit type signatures; generics are limited to top-level functions.
* Closures must have a single concrete type (no let-polymorphism); generics are expanded via monomorphization.
* The runtime uses garbage collection; manual ownership or lifetimes are not required.

### Builtin `HashMap`

* Builtin type: `HashMap[K, V]`, backed by generated Go runtime code and Go `map` internally.
* Builtin API: `hashmap_new`, `hashmap_get -> Option[V]`, `hashmap_set -> unit`, `hashmap_remove -> unit`, `hashmap_len -> int`, `hashmap_contains -> bool`.
* Key requirements: `K` must have both `Hash` and `Eq`.

### Builtin `Iterator`

* Builtin iterator protocol: `trait Iterator { type Item; fn next(Self) -> Option[Self::Item]; }`.
* Builtin conversion protocol: `trait IntoIterator { type Item; type IntoIter: Iterator; fn into_iter(Self) -> Self::IntoIter; }`. The compiler enforces `IntoIterator::Item == IntoIterator::IntoIter::Item`.
* `FnIterator[T]` is the standard closure-backed concrete iterator; user-defined stateful types can implement `Iterator` directly, and every iterator receives the identity `IntoIterator` implementation.
* Builtin API:
  * `FnIterator::from_fn(next_fn: () -> Option[T]) -> FnIterator[T]`
  * `Iterator::next(iterator) -> Option[Iterator::Item]` or `iterator.next()`
  * `Vec::[T]::iter() -> FnIterator[T]`
  * `Slice::[T]::iter() -> FnIterator[T]`
  * `range(start: int, end: int) -> FnIterator[int]`
  * `iterator_map`, `iterator_filter`, and `iterator_take` return concrete adapter iterator types
  * `iterator_fold` reduces an iterator and `iterator_collect` materializes it as `Vec[T]`
* `range` is half-open and increasing; `start >= end` produces an empty iterator.
* `Vec[T]` and `Slice[T]` implement `IntoIterator`, so `for value in values` and `for value in slice` work without an explicit `.iter()` conversion.

### Builtin `Slice`

* Builtin type: `Slice[T]`, a bounded read-only view over contiguous `Vec[T]` storage.
* Builtin API:
  * `slice(vec: Vec[T], start: int, end: int) -> Slice[T]`
  * `slice_get(s: Slice[T], index: int) -> T`
  * `slice_len(s: Slice[T]) -> int`
  * `slice_sub(s: Slice[T], start: int, end: int) -> Slice[T]`
* Inherent methods on `Slice[T]`: `get`, `len`, `sub`.
* Current limitation: mutable slice operations are intentionally not provided.

### Builtin traits `Eq` / `Hash`

* `trait Eq { fn eq(Self, Self) -> bool; }`
* `trait Hash { fn hash(Self) -> uint64; }`
* `Ref[T]` implements `Eq`/`Hash` by reference identity. Mutating the pointed-to value does not change equality or hashing.

### Testing / snapshots gotchas

* Adding/changing builtins changes the `builtin` interface hash, which can break `crates/goml/tests/expect/cli_commands_test/*`; update via `env UPDATE_EXPECT=1 cargo test`.
* Pipeline tests under `crates/compiler/src/tests/pipeline/` must only be updated via `env UPDATE_EXPECT=1 cargo test` (never hand-edit `.cst/.ast/.hir/.tast/.core/.mono/.anf/.go/.out`).
* Visibility tests live in `crates/compiler/src/tests/visibility_test.rs`; prefer focused typecheck fixtures there when checking `pub`/private module API behavior.

### Name collisions

* Avoid defining user traits named `Eq` or `Hash` in tests/examples unless you fully qualify or rename them; the builtins now reserve these names and duplicate impls can surface as “defined in multiple packages”.
