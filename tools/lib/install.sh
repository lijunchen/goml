#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
prefix="$1"

mkdir -p "$prefix/lib"
cp -R "$repo_root/lib/." "$prefix/lib/"
cp "$repo_root/lib/builtin/contract.gom" "$prefix/lib/builtin_contract.gom"
cp "$repo_root/lib/prelude/prelude.gom" "$prefix/lib/builtin_prelude.gom"
cp "$repo_root/lib/builtin/numeric.gom" "$prefix/lib/builtin_numeric.gom"
cp "$repo_root/lib/builtin/derive.gom" "$prefix/lib/builtin_derive.gom"
