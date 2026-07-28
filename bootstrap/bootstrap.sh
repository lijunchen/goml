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

tar -xzf "$stage0_archive" --strip-components=1 -C "$stage0_output/bin"
test -x "$stage0_output/bin/goml"
test -x "$stage0_output/bin/gomlc"
test -x "$stage0_output/bin/gomllsp"
test -f "$stage0_output/bin/builtin_prelude.gom"
test -f "$stage0_output/bin/lib/std/goml.toml"

stage0_version="${GOML_STAGE0_RELEASE_TAG#v}"
test "$("$stage0_output/bin/goml" version)" = "goml $stage0_version"
"$stage0_output/bin/gomlc" version --format json | grep -q '"driver_protocol":1'
