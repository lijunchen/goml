#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd "$(dirname "$0")/.." && pwd)
goml_root=${GOML_REPO:-"$project_root/../goml"}
parser="$project_root/artifact/bin/parser"
oracle="$project_root/artifact/rust-oracle/debug/gomlang-parser-rust-oracle"

goml build "$project_root"
CARGO_TARGET_DIR="$project_root/artifact/rust-oracle" cargo build \
  --quiet \
  --manifest-path "$project_root/tools/rust-oracle/Cargo.toml"

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
count=0

while IFS= read -r -d '' source; do
  if ! "$oracle" ast "$source" >"$work_dir/rust"; then
    printf 'Rust AST oracle failed: %s\n' "$source" >&2
    exit 1
  fi
  if ! "$parser" __canonical-stage ast "$source" >"$work_dir/goml"; then
    printf 'GoML AST parser failed: %s\n' "$source" >&2
    exit 1
  fi
  if ! cmp --silent "$work_dir/rust" "$work_dir/goml"; then
    printf 'AST mismatch: %s\n' "$source" >&2
    diff -u "$work_dir/rust" "$work_dir/goml" >&2 || true
    exit 1
  fi
  count=$((count + 1))
done < <(
  find \
    "$goml_root/crates/compiler/src/tests" \
    "$goml_root/crates/lexer" \
    "$goml_root/crates/parser" \
    "$goml_root/stdlib" \
    "$project_root" \
    -path '*/artifact' -prune -o \
    -path '*/target' -prune -o \
    -type f -name '*.gom' -print0
)

printf 'matched %d GoML AST corpus files\n' "$count"
