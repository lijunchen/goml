#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd "$(dirname "$0")/.." && pwd)
goml_root=${GOML_REPO:-"$(cd "$project_root/.." && pwd)"}
goml=${GOML_BIN:-"$goml_root/target/debug/goml"}
gomlc=${BOOTSTRAP_GOMLC_BIN:-"$project_root/artifact/bin/gomlc"}
pipeline_root=${GOML_PIPELINE_ROOT:-"$goml_root/crates/compiler/src/tests/pipeline"}

if [[ ${BOOTSTRAP_GOMLC_SKIP_BUILD:-0} != 1 ]]; then
  "$goml" build "$project_root"
fi

work_dir=$(mktemp -d)
trap 'rm -r "$work_dir"' EXIT

sources=("$pipeline_root"/*/main.gom)
stages=(cst ast hir tast core mono lift anf go)
matched=0

for stage in "${stages[@]}"; do
  printf 'checking %s snapshots\n' "$stage"
  for source in "${sources[@]}"; do
    expected="$source.$stage"
    actual="$work_dir/actual"
    errors="$work_dir/errors"
    if ! "$gomlc" "$stage" "$source" >"$actual" 2>"$errors"; then
      printf 'bootstrap gomlc failed: %s %s\n' "$stage" "$source" >&2
      sed 's/^/  /' "$errors" >&2
      exit 1
    fi
    if ! cmp --silent "$expected" "$actual"; then
      printf 'snapshot mismatch: %s\n' "$expected" >&2
      diff -u "$expected" "$actual" >&2 || true
      exit 1
    fi
    matched=$((matched + 1))
  done
done

printf 'checking runtime snapshots\n'
for source in "${sources[@]}"; do
  expected="$source.out"
  actual="$work_dir/actual"
  errors="$work_dir/errors"
  if ! "$gomlc" run-single "$source" >"$actual" 2>"$errors"; then
    printf 'bootstrap gomlc execution failed: %s\n' "$source" >&2
    sed 's/^/  /' "$errors" >&2
    exit 1
  fi
  if [[ -f "$expected" ]]; then
    if ! cmp --silent "$expected" "$actual"; then
      printf 'runtime mismatch: %s\n' "$expected" >&2
      diff -u "$expected" "$actual" >&2 || true
      exit 1
    fi
  elif [[ -s "$actual" ]]; then
    printf 'unexpected runtime output: %s\n' "$source" >&2
    diff -u /dev/null "$actual" >&2 || true
    exit 1
  fi
  matched=$((matched + 1))
done

printf 'matched %d pipeline snapshots across %d cases\n' "$matched" "${#sources[@]}"
