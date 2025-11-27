#!/usr/bin/env bash
set -euo pipefail

BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RSCRIPT="${RSCRIPT:-Rscript}"
JITTER="${JITTER:-$(( (RANDOM % 21) + 5 ))}"

# Optional jitter so multiple machines don't collide
sleep "${JITTER}s"

"${RSCRIPT}" "${BASE}/lcd_agent.R" --once
