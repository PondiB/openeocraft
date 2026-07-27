#!/usr/bin/env bash
# Close low-hanging openEO compliance issues after local verification.
# Requires: gh auth login  (once)
set -euo pipefail
REPO="Open-Earth-Monitor/openeocraft"

close_issue() {
  local num="$1"
  local body="$2"
  gh issue close "$num" -R "$REPO" --comment "$body"
  echo "Closed #$num"
}

close_issue 54 "$(cat <<'EOF'
Closing: L2 Recommended Data Processing is satisfied.

Both **synchronous** (`POST /result`) and **batch jobs** (`POST /jobs` + start/results) are implemented and covered by `tests/testthat/test-L1-minimal-data-processing.R` (and L1A/L1B suites). Secondary web services are not required when two of the three modes exist.
EOF
)"

close_issue 47 "$(cat <<'EOF'
Closing: Well-known discovery is implemented outside the versioned API tree.

`GET /.well-known/openeo` is registered in `docker/plumber.R` and handled by `api_wellknown.openeo_v1`, covered by `tests/testthat/test-L1-minimal-capabilities.R`.
EOF
)"

close_issue 63 "$(cat <<'EOF'
Closing: `GET /jobs` returns all jobs when `limit` is omitted, and supports optional `limit`/`page` pagination with `next`/`prev`/`first`/`last` links when `limit` is set.

Covered by `tests/testthat/test-L1B-minimal-batch-jobs.R`.
EOF
)"

close_issue 64 "$(cat <<'EOF'
Closing: `GET /processes` returns all processes when `limit` is omitted, and supports optional `limit`/`page` pagination with navigation links when `limit` is set.

Covered by `tests/testthat/test-L1-minimal-predefined-processes.R`.
EOF
)"

close_issue 38 "$(cat <<'EOF'
Closing: L1B Minimal Batch Jobs checklist is complete (including #63).

Create/list/info/delete/status behavior is covered by `tests/testthat/test-L1B-minimal-batch-jobs.R`. Responses include `jobs` and `links` arrays; optional `limit` pagination is supported.
EOF
)"

close_issue 44 "$(cat <<'EOF'
Closing: L1 Minimal Pre-defined Processes checklist is complete (including #64).

`GET /processes` returns `processes` and `links` (including `self`), works with/without auth, and supports optional `limit`. Covered by `tests/testthat/test-L1-minimal-predefined-processes.R`.
EOF
)"

close_issue 55 "$(cat <<'EOF'
Closing: L2 Recommended Batch Jobs matches the L1B surface already verified in #38.

Same endpoints and tests (`test-L1B-minimal-batch-jobs.R`), including optional `limit` pagination on `GET /jobs`.
EOF
)"

close_issue 36 "$(cat <<'EOF'
Closing: Process JSON decorator scaffolding is in place.

`#* @openeo-process` writes missing `processes/<id>.json` with `_generated_by: openeocraft` and never overwrites curated descriptors (`R/decorators.R`). Further multi-file `@openeo-import` work remains tracked in DEVELOPMENT.md if needed later.
EOF
)"

echo "All low-hanging issues closed."
