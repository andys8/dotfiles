#!/bin/sh
set -eu pipefail

./scripts/update-repository.sh

# Link files and directories
./scripts/link.sh

# Checks before
./scripts/check-preconditions.sh

# Updates
./scripts/update-nix.sh
./scripts/update-vim.sh
./scripts/update-fish.fish
./scripts/update-npm.sh
./scripts/update-xmonad.sh

# Abbreviations
./scripts/fish-abbreviations.fish

# Checks afterwards
./scripts/check-preconditions.sh
./scripts/check-postconditions.sh
