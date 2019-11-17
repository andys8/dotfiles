#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
  elm 
  elm-json
  elm-test
  markdownlint
  nmt # network-manager-tui
  pip3 # python packages
  prettier
  python3
  qutebrowser # Keyboard based webbrowser
  tsc
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
