#!/usr/bin/env bash
set -euo pipefail

BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RSCRIPT="${RSCRIPT:-Rscript}"

echo "Running ${RSCRIPT} ${BASE}/lcd_agent.R --once"
"${RSCRIPT}" "${BASE}/lcd_agent.R" --once
