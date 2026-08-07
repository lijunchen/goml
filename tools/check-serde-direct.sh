#!/usr/bin/env bash
set -euo pipefail

generated_file="${1:?generated Go file is required}"

function_body() {
    local pattern="$1"
    awk -v pattern="$pattern" '
        /^func / {
            if (printing) {
                exit
            }
            if (index($0, pattern) > 0) {
                printing = 1
                found = 1
            }
        }
        printing {
            print
        }
        END {
            if (!found) {
                exit 2
            }
        }
    ' "$generated_file"
}

assert_direct_body() {
    local pattern="$1"
    local body
    body="$(function_body "$pattern")"
    if rg -q '_goml_m_std_p_(serde|json)_p_(Value|Schema)|TypedSchema' <<<"$body"; then
        printf 'serde direct path contains a dynamic data-model constructor: %s\n' "$pattern" >&2
        exit 1
    fi
    if ! rg -q '_goml_m_trait__impl' <<<"$body"; then
        printf 'serde direct path is missing a statically selected trait implementation: %s\n' "$pattern" >&2
        exit 1
    fi
    if rg -q 'dyn__[[:alnum:]_]*(_vtable|__wrap)|\.vtable' <<<"$body"; then
        printf 'serde direct path contains Go interface dispatch: %s\n' "$pattern" >&2
        exit 1
    fi
}

assert_direct_body '_goml_m_std_p_json_p_stringify____T__benchmarks_p_serde_p_Record'
assert_direct_body '_goml_m_std_p_json_p_from__string____T__benchmarks_p_serde_p_Record'
assert_direct_body '_goml_m_std_p_bincode_p_encode__to__vec____T__benchmarks_p_serde_p_Record'
assert_direct_body '_goml_m_std_p_bincode_p_decode__from__slice____T__benchmarks_p_serde_p_Record'
