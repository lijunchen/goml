#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
prefix="$1"

mkdir -p "$prefix/lib"
rm -f \
    "$prefix/lib/builtin_contract.gom" \
    "$prefix/lib/builtin_prelude.gom" \
    "$prefix/lib/builtin_numeric.gom" \
    "$prefix/lib/builtin_derive.gom"
cp -R "$repo_root/lib/." "$prefix/lib/"
