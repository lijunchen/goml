set -euo pipefail

test "$#" = 2
test "$(uname -s)" = Linux
test "$(uname -m)" = x86_64

stage0_file="$1"
stage0_output="$2"
source "$stage0_file"

stage0_cache="$(dirname "$stage0_output")/cache"
stage0_archive="${GOML_STAGE0_ARCHIVE:-$stage0_cache/$GOML_STAGE0_ASSET_NAME}"
mkdir -p "$stage0_cache" "$stage0_output/bin"

if test -z "${GOML_STAGE0_ARCHIVE:-}"; then
    if ! test -f "$stage0_archive" || ! printf '%s  %s\n' "$GOML_STAGE0_SHA256" "$stage0_archive" | sha256sum --check --status; then
        stage0_temporary="$(mktemp "$stage0_cache/$GOML_STAGE0_ASSET_NAME.XXXXXX")"
        trap 'rm -f "$stage0_temporary"' EXIT
        curl --fail --location --retry 3 --output "$stage0_temporary" "$GOML_STAGE0_URL"
        printf '%s  %s\n' "$GOML_STAGE0_SHA256" "$stage0_temporary" | sha256sum --check --status
        mv "$stage0_temporary" "$stage0_archive"
        trap - EXIT
    fi
fi

printf '%s  %s\n' "$GOML_STAGE0_SHA256" "$stage0_archive" | sha256sum --check --status

for stage0_binary in goml gomlc; do
    stage0_member="$(tar -tzf "$stage0_archive" | grep -E "/$stage0_binary$")"
    test "$(printf '%s\n' "$stage0_member" | wc -l)" = 1
    stage0_temporary="$(mktemp "$stage0_output/bin/$stage0_binary.XXXXXX")"
    trap 'rm -f "$stage0_temporary"' EXIT
    tar -xOzf "$stage0_archive" "$stage0_member" > "$stage0_temporary"
    chmod 755 "$stage0_temporary"
    mv "$stage0_temporary" "$stage0_output/bin/$stage0_binary"
    trap - EXIT
done

stage0_version="${GOML_STAGE0_RELEASE_TAG#v}"
test "$("$stage0_output/bin/goml" version)" = "goml $stage0_version"
"$stage0_output/bin/gomlc" version --format json | grep -q '"driver_protocol":1'
