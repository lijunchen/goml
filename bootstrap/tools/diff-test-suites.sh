#!/usr/bin/env bash
set -euo pipefail

bootstrap_root=$(cd "$(dirname "$0")/.." && pwd)
goml_root=${GOML_REPO:-"$(cd "$bootstrap_root/.." && pwd)"}
goml=${GOML_BIN:-"$goml_root/target/debug/goml"}
rust_gomlc=${RUST_GOMLC_BIN:-"$goml_root/target/debug/gomlc"}
bootstrap_gomlc=${BOOTSTRAP_GOMLC_BIN:-"$bootstrap_root/artifact/bin/gomlc"}
oracle_manifest="$bootstrap_root/tools/rust-oracle/Cargo.toml"
oracle="$bootstrap_root/tools/rust-oracle/target/debug/gomlang-parser-rust-oracle"
tests_root="$goml_root/crates/compiler/src/tests"

if [[ ${BOOTSTRAP_GOMLC_SKIP_BUILD:-0} != 1 ]]; then
  "$goml" build "$bootstrap_root"
fi

cargo build --quiet --manifest-path "$oracle_manifest"

work_dir=$(mktemp -d)
trap 'rm -r "$work_dir"' EXIT
matched=0

compare_files() {
  local expected=$1
  local actual=$2
  local label=$3
  if ! cmp --silent "$expected" "$actual"; then
    printf 'mismatch: %s\n' "$label" >&2
    diff -u "$expected" "$actual" >&2 || true
    exit 1
  fi
  matched=$((matched + 1))
}

for suite in diagnostics typer; do
  while IFS= read -r -d '' source; do
    expected="$source.diag"
    actual="$work_dir/actual"
    "$bootstrap_gomlc" __test-diagnostics "$source" >"$actual"
    compare_files "$expected" "$actual" "$source"
  done < <(find "$tests_root/$suite" -maxdepth 1 -type f -name '*.gom' -print0 | sort -z)
done

while IFS= read -r -d '' source; do
  expected="$source.out"
  if [[ ! -f "$expected" ]]; then
    continue
  fi
  actual="$work_dir/actual"
  errors="$work_dir/errors"
  "$bootstrap_gomlc" __test-diagnostics "$source" >"$actual"
  if [[ ! -s "$actual" ]]; then
    if ! "$bootstrap_gomlc" run-single "$source" >"$actual" 2>"$errors"; then
      printf 'bootstrap execution failed: %s\n' "$source" >&2
      sed 's/^/  /' "$errors" >&2
      exit 1
    fi
  fi
  compare_files "$expected" "$actual" "$source"
done < <(find "$tests_root/e2e" -type f -name 'main.gom' -print0 | sort -z)

module_index=0
while IFS= read -r -d '' expected; do
  project=$(dirname "$expected")
  actual="$work_dir/actual"
  errors="$work_dir/errors"
  target_dir="$work_dir/module-artifact/$module_index"
  if ! (
    cd "$work_dir"
    "$goml" run \
      --compiler "$bootstrap_gomlc" \
      --target-dir "$target_dir" \
      "$project"
  ) >"$actual" 2>"$errors"; then
    printf 'bootstrap module execution failed: %s\n' "$project" >&2
    sed 's/^/  /' "$errors" >&2
    exit 1
  fi
  compare_files "$expected" "$actual" "$project"
  module_index=$((module_index + 1))
done < <(find "$tests_root/module" -type f -name 'main.gom.out' -print0 | sort -z)

stdio_fixture="$bootstrap_root/tools/fixtures/std_host_stdio"
if ! printf '\000\177\200\377' | env \
  -u GOML_MISSING_ENVIRONMENT_VALUE \
  GOML_EMPTY= \
  "$goml" run \
    --compiler "$bootstrap_gomlc" \
    --target-dir "$work_dir/stdio-artifact" \
    "$stdio_fixture" \
    >"$work_dir/stdio-output" \
    2>"$work_dir/stdio-errors"
then
  sed 's/^/  /' "$work_dir/stdio-errors" >&2
  exit 1
fi
stdio_bytes=$(od -An -v -t u1 "$work_dir/stdio-output" | tr -s ' ' | tr -d '\n' | sed 's/^ //')
if [[ $stdio_bytes != '0 127 128 255' ]]; then
  printf 'stdio output mismatch: %s\n' "$stdio_bytes" >&2
  exit 1
fi
if [[ $(<"$work_dir/stdio-errors") != 'binary stderr' ]]; then
  sed 's/^/  /' "$work_dir/stdio-errors" >&2
  exit 1
fi
matched=$((matched + 1))

while IFS= read -r -d '' source; do
  project=$(dirname "$source")
  if [[ -f "$project/goml.toml" ]]; then
    continue
  fi
  rust_diagnostics="$work_dir/rust-diagnostics"
  bootstrap_diagnostics="$work_dir/bootstrap-diagnostics"
  "$oracle" diagnostics "$source" >"$rust_diagnostics"
  "$bootstrap_gomlc" __test-diagnostics "$source" >"$bootstrap_diagnostics"
  compare_files "$rust_diagnostics" "$bootstrap_diagnostics" "$source diagnostics"
  if [[ -s "$rust_diagnostics" ]]; then
    continue
  fi
  rust_output="$work_dir/rust-output"
  bootstrap_output="$work_dir/bootstrap-output"
  if "$rust_gomlc" run-single "$source" >"$rust_output" 2>"$work_dir/rust-errors"; then
    rust_exit=0
  else
    rust_exit=$?
  fi
  if "$bootstrap_gomlc" run-single "$source" >"$bootstrap_output" 2>"$work_dir/bootstrap-errors"; then
    bootstrap_exit=0
  else
    bootstrap_exit=$?
  fi
  if [[ $rust_exit -ne $bootstrap_exit ]]; then
    printf 'execution status mismatch: %s Rust=%d bootstrap=%d\n' \
      "$source" "$rust_exit" "$bootstrap_exit" >&2
    exit 1
  fi
  compare_files "$rust_output" "$bootstrap_output" "$source runtime"
done < <(find "$tests_root/crashers" -type f -name 'main.gom' -print0 | sort -z)

host_shadow="$tests_root/crashers/local_std_host_extern_shadow"
if (
  cd "$work_dir"
  "$goml" check \
    --compiler "$bootstrap_gomlc" \
    --target-dir "$work_dir/host-shadow-artifact" \
    "$host_shadow"
) >"$work_dir/host-shadow-output" 2>&1; then
  printf 'bootstrap unexpectedly accepted local std host extern\n' >&2
  exit 1
fi
if ! rg --quiet 'extern args_raw is not permitted in this source' "$work_dir/host-shadow-output"; then
  sed 's/^/  /' "$work_dir/host-shadow-output" >&2
  exit 1
fi
matched=$((matched + 1))

printf 'matched %d diagnostics, typer, e2e, module, and crasher results\n' "$matched"
