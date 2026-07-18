#!/usr/bin/env bash
set -euo pipefail

project_root=$(cd "$(dirname "$0")/.." && pwd)
parser="$project_root/artifact/bin/parser"
target_dir="$project_root/artifact/rust-oracle"

goml build "$project_root"
CARGO_TARGET_DIR="$target_dir" cargo build \
  --quiet \
  --manifest-path "$project_root/tools/rust-oracle/Cargo.toml"

"$target_dir/debug/diff" "$parser" 4096
"$target_dir/debug/diff_parser" "$parser" cst 2048
"$target_dir/debug/diff_parser" "$parser" ast 2048
