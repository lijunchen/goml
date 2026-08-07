#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
bench_dir="$repo_root/benchmarks/serde"
target_dir="$(mktemp -d)"
generated_dir="$target_dir/build/pkg/benchmarks/serde"
generated_file="$generated_dir/goml_generated.go"
executable_file="$target_dir/bin/serde"
trap 'rm -rf "$target_dir"' EXIT

cd "$bench_dir"
"$repo_root/stage2/bin/goml" build --target-dir "$target_dir"
bash "$repo_root/tools/check-serde-direct.sh" "$generated_file"
cp benchmark_test.go "$generated_dir/benchmark_test.go"
cd "$generated_dir"
generated_bytes="$(wc -c < "$generated_file" | tr -d ' ')"
generated_functions="$(rg -c '^func ' "$generated_file")"
executable_bytes="$(wc -c < "$executable_file" | tr -d ' ')"
printf 'serde generated Go: %s bytes, %s functions\n' "$generated_bytes" "$generated_functions"
printf 'serde benchmark executable: %s bytes\n' "$executable_bytes"
started_ns="$(date +%s%N)"
go test -run '^$' -bench . -benchmem goml_generated.go benchmark_test.go "$@"
finished_ns="$(date +%s%N)"
printf 'serde benchmark wall: %s ns\n' "$((finished_ns - started_ns))"
