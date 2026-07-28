set -euo pipefail

release_script="$(dirname "$0")/release.sh"

bash "$release_script" check-next v0.1.0 v0.1.1
bash "$release_script" check-next v0.1.0 v0.2.0
bash "$release_script" check-next v0.1.0 v1.0.0
! bash "$release_script" check-next v0.1.0 v0.1.2
! bash "$release_script" check-next v0.1.0 v0.3.0
test "$(bash "$release_script" latest v0.1.9 v0.2.0 v0.1.10)" = v0.2.0
