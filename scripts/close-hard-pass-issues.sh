#!/usr/bin/env bash
# Close remaining compliance issues implemented in the hard-pass (except CRAN #83).
set -euo pipefail
REPO="Open-Earth-Monitor/openeocraft"

close_issue() {
  local num="$1"
  local body="$2"
  gh issue close "$num" -R "$REPO" --comment "$body"
  echo "Closed #$num"
}

close_issue 43 "$(cat <<'EOF'
Closing: L1 Authentication checklist is implemented.

- Bearer tokens accept `method/identityProviderId/token` (and legacy bare tokens).
- Missing auth → **401**; invalid/expired → **403**.
- `GET /credentials/basic` remains supported.
Covered by `tests/testthat/test-L1-auth-and-account.R`.
EOF
)"

close_issue 37 "$(cat <<'EOF'
Closing: L1A Synchronous Processing checklist is implemented.

`POST /result` validates process graphs, returns **200** on success and **4xx** on failure, sets suitable `Content-Type` (including GeoTIFF / NetCDF), and rejects non-free plans via the payment gate.
Covered by `tests/testthat/test-L1A-minimal-synchronous-processing.R`.
EOF
)"

close_issue 58 "$(cat <<'EOF'
Closing: L2 Recommended Synchronous Processing matches the L1A surface closed in #37.
EOF
)"

close_issue 56 "$(cat <<'EOF'
Closing: Batch job logs support offset/limit, return `logs`+`links`, expose level/time, use unique ids, and keep messages concise (no stack dumps).
Covered by `tests/testthat/test-L1B-results-and-logs.R`.
EOF
)"

close_issue 39 "$(cat <<'EOF'
Closing: L1B Batch Jobs > Results checklist is implemented.

- `POST/GET/DELETE /jobs/{id}/results` (start / fetch / cancel)
- Unfinished jobs without `partial=true` → openEO `JobNotFinished`
- STAC Collection responses with `stac_version`, asset titles, extents, `expires`, and `canonical` link
- Payment gate on start for non-free plans
Covered by `tests/testthat/test-L1B-results-and-logs.R`.
EOF
)"

close_issue 57 "$(cat <<'EOF'
Closing: L2 Recommended Batch Jobs > Results matches the L1B surface closed in #39.
EOF
)"

close_issue 51 "$(cat <<'EOF'
Closing: `GET /me` returns a unique `user_id` for authenticated users.
Covered by `tests/testthat/test-L1-auth-and-account.R`.
EOF
)"

close_issue 48 "$(cat <<'EOF'
Closing: Landing page (`GET /`) now includes a `billing` object with currency, default free plan, and plan list (no metering yet).
EOF
)"

close_issue 50 "$(cat <<'EOF'
Closing: `GET /credentials/oidc` discovery is supported without authentication and returns `{ "providers": [] }` until an IdP is configured. Full OIDC login remains a follow-up (see DEVELOPMENT.md).
EOF
)"

close_issue 52 "$(cat <<'EOF'
Closing: All shipped `inst/ml/processes/*.json` descriptors provide non-empty `summary` and `categories`, verified by `tests/testthat/test-L2-processes-and-parameters.R`. Process JSON is curated to match the ML implementation set.
EOF
)"

close_issue 53 "$(cat <<'EOF'
Closing: Collections metadata updated in `inst/ml/db.rds` — titles, keywords, proprietary license links, and datacube `stac_extensions`. Served via openSTAC `GET /collections` / `GET /collections/{id}`.
EOF
)"

close_issue 8 "$(cat <<'EOF'
Closing: Exported API surface is documented via roxygen (`man/*.Rd`, NAMESPACE). Remaining CRAN-oriented polish stays tracked under #83.
EOF
)"

# Leave #46 open with a progress comment (UDP/namespaces still deferred)
gh issue comment 46 -R "$REPO" --body "$(cat <<'EOF'
Progress update: `from_parameter` is supported and covered by tests (`arg_type` → `parameter_reference`).

Still open / deferred:
- openEO process namespaces (`namespace:process_id`)
- Stored user-defined processes (`process_id` UDP) and `/process_graphs` CRUD

See DEVELOPMENT.md roadmap.
EOF
)"

echo "Done closing implemented issues (CRAN #83 and UDP #46 remain open)."
