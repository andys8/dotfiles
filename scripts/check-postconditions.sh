#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
	"chromium-browser" # web browser
	"elm" # elm compiler
	"elm-json" # elm installation helper
	"elm-test" # elm test runner
	"ghcide" # haskell language server
	"git-brunch" # branch change tool
	"git-brunch" # git tool
	"hlint" # haskell linter
	"htop" # htop (hopefully with vim bindings)
	"lock" # bin/lock file is on the path
	"nmt" # network-manager-tui
	"prettier" # js/css/markdown formatter
	"qrcode-terminal" # show qr codes
	"st" # suckless terminal
	"tsc" # typescript compiler
)

fail() {
	echo "ERROR: $1"
	exit 1
}

check() {
	command -v "$1" >/dev/null 2>&1 || {
		fail "Command '$1' missing"
	}
}

# Check commands
for i in "${commands[@]}"; do check "$i"; done

# autorandr config
[[ $(autorandr --current) == "" ]] && {
	fail "autorandr needs to be setup with configurations"
}

# preload
[[ $(systemctl is-active preload) != "active" ]] && {
	fail "service 'preload' not active (systemctl enable preload)"
}

# Otherwise good case
echo "All postconditions are fine"
exit 0
