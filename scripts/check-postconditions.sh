#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
  chromium-browser # web browser
  elm # elm compiler
  elm-json # elm installation helper
  elm-test # elm test runner
  git-brunch # branch change tool
  i3lock-fancy # lockscreen
  markdownlint # markdown analysis
  nmt # network-manager-tui
  prettier # js/css/markdown formatter
  tsc # typescript compiler
)

check() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "'$1' missing"
    exit 1
  }
}


# Check commands
for i in "${commands[@]}"; do check "$i"; done

# Otherwise good case
echo "All postconditions are fine"
exit 0
