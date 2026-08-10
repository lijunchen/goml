#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

prefix="$(cd "$1" && pwd)"
temporary="$(mktemp -d)"
trap 'rm -rf "$temporary"' EXIT

test -f "$prefix/lib/builtin/contract.gom"
test -f "$prefix/lib/prelude/prelude.gom"
test -f "$prefix/lib/std/goml.toml"
test -f "$prefix/lib/builtin_contract.gom"
test -f "$prefix/lib/builtin_prelude.gom"
test -f "$prefix/lib/builtin_numeric.gom"
test -f "$prefix/lib/builtin_derive.gom"

mkdir -p "$temporary/bin"
cp "$prefix/bin/gomlc" "$temporary/bin/gomlc"

if "$temporary/bin/gomlc" __builtin-interface > "$temporary/stdout" 2> "$temporary/stderr"; then
    exit 1
fi

grep -F "could not find builtin and prelude resources in $temporary/lib" "$temporary/stderr" >/dev/null
grep -F "builtin/contract.gom" "$temporary/stderr" >/dev/null
grep -F "builtin_contract.gom" "$temporary/stderr" >/dev/null

cd "$temporary"
"$prefix/bin/gomlc" __builtin-interface >/dev/null

mkdir -p "$temporary/legacy/bin" "$temporary/legacy/lib"
cp "$prefix/bin/gomlc" "$temporary/legacy/bin/gomlc"
cp "$prefix/lib/builtin_contract.gom" "$temporary/legacy/lib/builtin_contract.gom"
cp "$prefix/lib/builtin_prelude.gom" "$temporary/legacy/lib/builtin_prelude.gom"
cp "$prefix/lib/builtin_numeric.gom" "$temporary/legacy/lib/builtin_numeric.gom"
cp "$prefix/lib/builtin_derive.gom" "$temporary/legacy/lib/builtin_derive.gom"
"$temporary/legacy/bin/gomlc" __builtin-interface >/dev/null

mkdir -p "$temporary/incomplete/bin" "$temporary/incomplete/lib/builtin"
cp "$prefix/bin/gomlc" "$temporary/incomplete/bin/gomlc"
cp "$prefix/lib/builtin_contract.gom" "$temporary/incomplete/lib/builtin_contract.gom"
cp "$prefix/lib/builtin_prelude.gom" "$temporary/incomplete/lib/builtin_prelude.gom"
cp "$prefix/lib/builtin_numeric.gom" "$temporary/incomplete/lib/builtin_numeric.gom"
cp "$prefix/lib/builtin_derive.gom" "$temporary/incomplete/lib/builtin_derive.gom"
cp "$prefix/lib/builtin/contract.gom" "$temporary/incomplete/lib/builtin/contract.gom"

if "$temporary/incomplete/bin/gomlc" __builtin-interface > "$temporary/incomplete/stdout" 2> "$temporary/incomplete/stderr"; then
    exit 1
fi

grep -F "incomplete current builtin and prelude resource layout" "$temporary/incomplete/stderr" >/dev/null
