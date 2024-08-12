#!/bin/bash
set -euo pipefail

# Asserted commands (should be installed by script)
assertCommands=(
    ffmpeg          # convert videos (used in scripts)
    git-brunch      # branch change tool
    htop            # htop (hopefully with vim bindings)
    lock            # bin/lock file is on the path
    prettier        # js/css/markdown formatter
    qrcode-terminal # show qr codes
    sgpt            # TheR1D/shell_gpt
    tsc             # typescript compiler
)

# Optional user commands (installed manually)
commandsOptional=(
    alacritty       # terminal-emulator
    btm             # bottom process monitor
    chromium        # chromium web browser
    idea            # intellij ide
    java            # java runtime
    json-tui        # TUI to visualize json input
    thorium-browser # web browser
    trash-put       # trash-cli
    shfmt           # shell format
    uuidgen         # generate uuid
    vimdiff         # git diffs in vim
)

errors=0
warnings=0

warn() {
    echo "$(tput setaf 3)WARNING:$(tput sgr 0) $1"
    warnings=$((warnings + 1))
}

fail() {
    echo "$(tput setaf 1)ERROR:$(tput sgr 0) $1"
    errors=$((errors + 1))
}

commandExists() {
    command -v "$1" >/dev/null 2>&1 || {
        fail "Command '$1' missing"
    }
}

warnIfCommandMissing() {
    command -v "$1" >/dev/null 2>&1 || {
        warn "Command '$1' missing"
    }
}

# Check commands
for i in "${assertCommands[@]}"; do commandExists "$i"; done

# Check commands
for i in "${commandsOptional[@]}"; do warnIfCommandMissing "$i"; done

# Result
[[ $warnings != 0 ]] && echo "$(tput setaf 3)Post-Conditions have $warnings warning(s)$(tput sgr 0)"
if [[ $errors != 0 ]]; then
    echo "$(tput setaf 1)Post-Conditions failed with $errors error(s)$(tput sgr 0)" && exit 1
else
    echo "$(tput setaf 2)Post-Conditions are fine$(tput sgr 0)" && exit 0
fi
