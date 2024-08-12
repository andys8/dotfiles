#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
    awk     # used in shell scripts
    bash    # shell
    diff    # compare files
    fc-list # list fonts
    find    # used in scripts
    fish    # shell
    node    # node
    npm     # node package manager
    pgrep   # grep processes
    pip3    # python packages
    python3 # pythong
    tar     # archive
    tr      # used in shell scripts
    vim     # editor
    xargs   # scripting
    xclip   # copy/paste
)

fonts=(
    "Font Awesome 6 Free Solid"
    "Iosevka Nerd"
    "Noto Color Emoji"
)

errors=0
warnings=0

fail() {
    echo "$(tput setaf 1)ERROR:$(tput sgr 0) $1"
    errors=$((errors + 1))
}

commandExists() {
    command -v "$1" >/dev/null 2>&1 || {
        fail "Command '$1' missing"
    }
}

# Check commands
for i in "${commands[@]}"; do commandExists "$i"; done

# Vim flags
[[ $(vim --version) =~ \+clipboard ]] || {
    fail "Vim should have '+clipboard'"
}

[[ $(vim --version) =~ \+python3 ]] || {
    fail "Vim should have '+python3'"
}

# Check fonts
fontInstalled() {
    fc-list | grep -i "$1" >/dev/null 2>&1 || {
        fail "Font '$1' missing"
    }
}

for i in "${fonts[@]}"; do fontInstalled "$i"; done

# Result
[[ $warnings != 0 ]] && echo "$(tput setaf 2)Pre-Conditions have $warnings warning(s)$(tput sgr 0)"
if [[ $errors != 0 ]]; then
    echo "$(tput setaf 1)Pre-Conditions failed with $errors error(s)$(tput sgr 0)" && exit 1
else
    echo "$(tput setaf 2)Pre-Conditions are fine$(tput sgr 0)" && exit 0
fi
