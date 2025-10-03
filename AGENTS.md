# Repository Guidelines

## Project Snapshot
- Go/Rust-inspired language frontend lowers through `lexer → parser → CST → AST → typed AST → Core → Mono → ANF` before emitting Go (`crates/compiler/src/go`).
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
- Lint (Rust): `just clippy` (equivalent to `cargo clippy --all-targets --all-features --locked -- -D warnings`).
- Format (Rust): `cargo fmt`.
- Wasm build: `wasm-pack build ./crates/wasm-app`.
- Webapp dev: `just start` (build Wasm, then `pnpm install` and `pnpm run dev` in `webapp`).
- Webapp build/preview: `pnpm -C webapp build` and `pnpm -C webapp preview`.
- CI locally: cargo check, test, fmt, clippy.

## Coding Style & Naming Conventions
- Rust (edition 2024): format with `rustfmt`; deny clippy warnings. Use snake_case for functions/modules, CamelCase for types, SCREAMING_SNAKE_CASE for consts.
- TypeScript/React: 2‑space indent; PascalCase components; named exports preferred. Lint with `pnpm -C webapp lint`.
- Keep modules small and focused; avoid cross‑crate leakage—public APIs live in each crate’s `lib.rs`.

## Testing Guidelines
- We use expect-test, there are two basic commands:
  - `cargo test` run test to match golden snapshots
  - `env UPDATE_EXPECT=1 cargo test` to update snapshots
- Aim for fast, deterministic tests; cover parsing/typing edges with minimal fixtures in `crates/compiler/src/tests/examples/` when relevant.
- You can pick some cases in crates/compiler/src/tests/cases, and use cargo run to quick check a test case, such as: `cargo run -- crates/compiler/src/tests/cases/001.src`.
- `crates/compiler/src/tests/cases/*.src` is input file. You should NEVER modify other files in crates/compiler/src/tests/cases, the only way to update them is by command: `env UPDATE_EXPECT=1 cargo test`.
- When I ask you to “add pipeline tests”, create a new `xxx.src` file under `crates/compiler/src/tests/cases`.

## Commit & Pull Request Guidelines
- Prefer Conventional Commits (`feat:`, `fix:`, `refactor:`, `chore:`). Be concise and imperative: “add parser error for …”.
- PRs: include a clear description, linked issues, and before/after notes or screenshots for web UI changes.
- Required: run cargo check, test, fmt, clippy locally; ensure no clippy or fmt diffs.

## Environment & Tooling
- Requirements: Rust toolchain, `wasm-pack`, Node 18+, `pnpm`.
- If adding crates, update workspace in `Cargo.toml`; keep inter‑crate deps via `[workspace.dependencies]`.
