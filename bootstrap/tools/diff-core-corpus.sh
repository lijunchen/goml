#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd "$(dirname "$0")/.." && pwd)
goml_root=${GOML_REPO:-"$project_root/../goml"}
parser=${GOMLANG_PARSER_BIN:-"$project_root/artifact/bin/parser"}
oracle="$project_root/artifact/rust-oracle/debug/gomlang-parser-rust-oracle"
diff_lines=${GOMLANG_CORE_DIFF_LINES:-0}

if [[ ${GOMLANG_PARSER_SKIP_BUILD:-0} != 1 ]]; then
  goml build "$project_root"
fi
CARGO_TARGET_DIR="$project_root/artifact/rust-oracle" cargo build \
  --quiet \
  --manifest-path "$project_root/tools/rust-oracle/Cargo.toml"

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
count=0

while IFS= read -r -d '' source; do
  if ! "$oracle" core "$source" >"$work_dir/rust"; then
    printf 'Rust Core oracle failed: %s\n' "$source" >&2
    exit 1
  fi
  if [[ ! -s "$work_dir/rust" ]]; then
    continue
  fi
  if ! "$parser" core "$source" >"$work_dir/goml"; then
    printf 'GoML Core lowering failed: %s\n' "$source" >&2
    exit 1
  fi
  if ! cmp --silent "$work_dir/rust" "$work_dir/goml"; then
    printf 'Core mismatch after %d matched files: %s\n' "$count" "$source" >&2
    if [[ $diff_lines -gt 0 ]]; then
      diff -u "$work_dir/rust" "$work_dir/goml" | head -n "$diff_lines" >&2 || true
    else
      diff -u "$work_dir/rust" "$work_dir/goml" >&2 || true
    fi
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

printf 'matched %d GoML Core corpus files\n' "$count"
