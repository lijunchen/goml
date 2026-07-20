#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd "$(dirname "$0")/.." && pwd)
goml_root=${GOML_REPO:-"$project_root/../goml"}
parser=${GOMLANG_PARSER_BIN:-"$project_root/artifact/bin/parser"}
gomlc=${GOMLC_BIN:-"$goml_root/target/debug/gomlc"}
source_file=${GOMLANG_RUN_SINGLE_SOURCE:-"$goml_root/crates/compiler/src/tests/pipeline/001/main.gom"}

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT

flags=(
  --dump-ast
  --dump-hir
  --dump-tast
  --dump-core
  --dump-mono
  --dump-lift
  --dump-anf
  --dump-go
)

"$gomlc" run-single "${flags[@]}" "$source_file" >"$work_dir/rust"
"$parser" run-single "${flags[@]}" "$source_file" >"$work_dir/goml"

if ! cmp --silent "$work_dir/rust" "$work_dir/goml"; then
  diff -u "$work_dir/rust" "$work_dir/goml" >&2 || true
  exit 1
fi

printf 'run-single output matches Rust gomlc\n'
