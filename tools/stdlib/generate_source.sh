#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
output="$repo_root/gomlc/stdlib/source.gom"
temporary="$(mktemp)"
builtin_output="$repo_root/gomlc/core/builtin_source.gom"
builtin_temporary="$(mktemp)"
trap 'rm -f "$temporary" "$builtin_temporary"' EXIT

append_source() {
    local name="$1"
    shift
    printf 'pub fn %s_source() -> string {\n' "$name" >> "$temporary"
    printf '    let source = ' >> "$temporary"
    local first_file=true
    for source_path in "$@"; do
        local line_number=0
        while IFS= read -r line || [[ -n "$line" ]]; do
            line_number=$((line_number + 1))
            if [[ "$first_file" == false && "$line_number" == 1 ]]; then
                continue
            fi
            if [[ "$first_file" == true && "$line_number" == 1 ]]; then
                printf '\\\\%s\n' "$line" >> "$temporary"
            else
                printf '        \\\\%s\n' "$line" >> "$temporary"
            fi
        done < "$repo_root/$source_path"
        first_file=false
    done
    printf '    ;\n    source\n}\n\n' >> "$temporary"
}

printf 'package stdlib;\n\n' > "$temporary"
append_source root stdlib/std/lib.gom
append_source host stdlib/std/internal/host/host.gom
append_source bytes stdlib/std/bytes/bytes.gom
append_source fs stdlib/std/fs/fs.gom
append_source io stdlib/std/io/io.gom
append_source env stdlib/std/env/env.gom
append_source path stdlib/std/path/path.gom
append_source process stdlib/std/process/process.gom
append_source testing stdlib/std/testing/testing.gom
append_source collections \
    stdlib/std/collections/arena.gom \
    stdlib/std/collections/bit_set.gom \
    stdlib/std/collections/deque.gom \
    stdlib/std/collections/hash_set.gom \
    stdlib/std/collections/index_map.gom \
    stdlib/std/collections/index_vec.gom \
    stdlib/std/collections/interner.gom \
    stdlib/std/collections/stack.gom \
    stdlib/std/collections/algorithms.gom
append_source json stdlib/std/json/json.gom
append_source text \
    stdlib/std/text/string_builder.gom \
    stdlib/std/text/text.gom
append_source num stdlib/std/num/num.gom
append_source time stdlib/std/time/time.gom

sed -i '$d' "$temporary"
mv "$temporary" "$output"

printf 'package core;\n\n' > "$builtin_temporary"
printf 'pub fn builtin_source() -> string {\n' >> "$builtin_temporary"
printf '    let source = \\\\package builtin;\n' >> "$builtin_temporary"
printf '        \\\\\n' >> "$builtin_temporary"
for source_path in stdlib/builtin_contract.gom stdlib/builtin_prelude.gom; do
    while IFS= read -r line || [[ -n "$line" ]]; do
        printf '        \\\\%s\n' "$line" >> "$builtin_temporary"
    done < "$repo_root/$source_path"
    printf '        \\\\\n' >> "$builtin_temporary"
done
printf '    ;\n    source\n}\n' >> "$builtin_temporary"
mv "$builtin_temporary" "$builtin_output"
trap - EXIT
