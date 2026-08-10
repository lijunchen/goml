#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

prefix="$(cd "$1" && pwd)"
temporary="$(mktemp -d)"
trap 'rm -rf "$temporary"' EXIT

test -f "$prefix/lib/builtin/contract.gom"
test -f "$prefix/lib/builtin/runtime.gom"
test -f "$prefix/lib/builtin/impls.gom"
test -f "$prefix/lib/builtin/language.gom"
test -f "$prefix/lib/builtin/numeric.gom"
test -f "$prefix/lib/builtin/derive.gom"
test -f "$prefix/lib/prelude/prelude.gom"
test -f "$prefix/lib/std/goml.toml"
test ! -e "$prefix/lib/builtin_contract.gom"
test ! -e "$prefix/lib/builtin_prelude.gom"
test ! -e "$prefix/lib/builtin_numeric.gom"
test ! -e "$prefix/lib/builtin_derive.gom"

mkdir -p "$temporary/bin"
cp "$prefix/bin/gomlc" "$temporary/bin/gomlc"

if "$temporary/bin/gomlc" __builtin-interface > "$temporary/stdout" 2> "$temporary/stderr"; then
    exit 1
fi

grep -F "could not read builtin resource $temporary/lib/builtin/contract.gom" "$temporary/stderr" >/dev/null
grep -F "builtin/contract.gom" "$temporary/stderr" >/dev/null

cd "$temporary"
cp -R "$prefix/lib" "$temporary/lib"
"$temporary/bin/gomlc" __builtin-interface >/dev/null
"$temporary/bin/gomlc" __prelude-interface >/dev/null

mv "$temporary/lib/prelude/prelude.gom" "$temporary/lib/prelude/prelude.gom.missing"
if "$temporary/bin/gomlc" __prelude-interface > "$temporary/prelude-stdout" 2> "$temporary/prelude-stderr"; then
    exit 1
fi
grep -F "could not read prelude resource $temporary/lib/prelude/prelude.gom" "$temporary/prelude-stderr" >/dev/null
