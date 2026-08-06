#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
prefix="$1"

mkdir -p "$prefix/lib/std"
cp "$repo_root/stdlib/builtin_contract.gom" "$prefix/lib/builtin_contract.gom"
cp "$repo_root/stdlib/builtin_prelude.gom" "$prefix/lib/builtin_prelude.gom"
cp "$repo_root/stdlib/builtin_derive.gom" "$prefix/lib/builtin_derive.gom"
cp -R "$repo_root/stdlib/std/." "$prefix/lib/std/"
