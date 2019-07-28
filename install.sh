#!/bin/sh
set -eu pipefail

# Link files and directories
./scripts/link.sh

# Checks
./scripts/check-preconditions.sh

# Updates
./scripts/update-nix.sh
./scripts/update-vim.sh
./scripts/update-fish.fish
./scripts/update-npm.sh

# Abbreviations
./scripts/fish-abbreviations.fish
