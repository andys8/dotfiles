#!/bin/sh
set -eu pipefail

git pull

# Link files and directories
./scripts/link.sh

# Checks
./scripts/check-preconditions.sh

# Updates
./scripts/update-nix.sh
./scripts/update-vim.sh
./scripts/update-fish.fish
./scripts/update-npm.sh
./scripts/update-xmonad.sh

# Abbreviations
./scripts/fish-abbreviations.fish

# Checks
./scripts/check-postconditions.sh
