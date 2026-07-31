set -euo pipefail

lsp_binary="$1"
lsp_version="$2"
lsp_output="$(mktemp)"
trap 'rm -f "$lsp_output"' EXIT

initialize='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
initialized='{"jsonrpc":"2.0","method":"initialized","params":{}}'
shutdown='{"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}'
exit_message='{"jsonrpc":"2.0","method":"exit","params":null}'

{
    printf 'Content-Length: %d\r\n\r\n%s' "${#initialize}" "$initialize"
    printf 'Content-Length: %d\r\n\r\n%s' "${#initialized}" "$initialized"
    printf 'Content-Length: %d\r\n\r\n%s' "${#shutdown}" "$shutdown"
    printf 'Content-Length: %d\r\n\r\n%s' "${#exit_message}" "$exit_message"
} | "$lsp_binary" > "$lsp_output"

grep -q "\"version\":\"$lsp_version\"" "$lsp_output"
grep -q '"documentFormattingProvider":true' "$lsp_output"
grep -q '"id":2,"result":null' "$lsp_output"
