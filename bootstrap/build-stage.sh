set -euo pipefail

test "$#" = 3

stage_name="$1"
driver_path="$(realpath "$2")"
compiler_path="$(realpath "$3")"
repository_root="$(cd "$(dirname "$0")/.." && pwd)"

(
    cd "$repository_root/gomlc"
    "$driver_path" build --target-dir "_bootstrap/$stage_name" --compiler "$compiler_path"
) &
compiler_build_pid="$!"

(
    cd "$repository_root/goml"
    "$driver_path" build --target-dir "_bootstrap/$stage_name" --compiler "$compiler_path"
) &
driver_build_pid="$!"

set +e
wait "$compiler_build_pid"
compiler_build_status="$?"
wait "$driver_build_pid"
driver_build_status="$?"
set -e

test "$compiler_build_status" = 0
test "$driver_build_status" = 0
