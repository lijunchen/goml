#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

prefix="$(cd "$1" && pwd)"
temporary="$(mktemp -d)"
trap 'rm -rf "$temporary"' EXIT

mkdir -p "$temporary/bin"
cp "$prefix/bin/gomlc" "$temporary/bin/gomlc"

if "$temporary/bin/gomlc" __builtin-interface > "$temporary/stdout" 2> "$temporary/stderr"; then
    exit 1
fi

grep -F "$temporary/lib/builtin_contract.gom" "$temporary/stderr" >/dev/null

cd "$temporary"
"$prefix/bin/gomlc" __builtin-interface >/dev/null
