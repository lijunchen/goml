#!/usr/bin/env bash

set -euo pipefail

test "$#" = 1

repository_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
release_version="$1"
package="goml-$release_version-linux-amd64"
smoke_root="$(mktemp -d)"
trap 'rm -rf "$smoke_root"' EXIT

tar -xzf "$repository_root/dist/$package.tar.gz" -C "$smoke_root"
test -f "$smoke_root/$package/lib/builtin/contract.gom"
test -f "$smoke_root/$package/lib/builtin/goml.toml"
test -f "$smoke_root/$package/lib/builtin/runtime.gom"
test -f "$smoke_root/$package/lib/builtin/impls.gom"
test -f "$smoke_root/$package/lib/builtin/language.gom"
test -f "$smoke_root/$package/lib/builtin/numeric.gom"
test -f "$smoke_root/$package/lib/builtin/derive.gom"
test -f "$smoke_root/$package/lib/prelude/prelude.gom"
test -f "$smoke_root/$package/lib/prelude/goml.toml"
test -f "$smoke_root/$package/lib/std/goml.toml"
test ! -e "$smoke_root/$package/lib/compiler/compiler-world-v2.gaf"
"$smoke_root/$package/bin/goml" __toolchain-finalize --prefix "$smoke_root/$package"
test -f "$smoke_root/$package/lib/compiler/compiler-world-v2.gaf"
cp -R "$repository_root/tools/release/testdata/smoke" "$smoke_root/project"
cd "$smoke_root/project"
"$smoke_root/$package/bin/goml" check
"$smoke_root/$package/bin/goml" build
test "$("$smoke_root/$package/bin/goml" run)" = "std/works"
"$smoke_root/$package/bin/goml" test
"$smoke_root/$package/bin/gomlfmt" -w ./*.gom
"$smoke_root/$package/bin/gomlfmt" --check ./*.gom
"$smoke_root/$package/bin/goml" fmt --check
bash "$repository_root/tools/release/lsp_smoke.sh" "$smoke_root/$package/bin/gomllsp" "$release_version"
