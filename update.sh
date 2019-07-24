#!/bin/bash
set -euo pipefail

# Nix
nix-channel --update
nix-env -irf manifest.nix

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean! +qall

# Npm installable dependencies
npminstallations=(
  markdownlint-cli
  prettier
  typescript
)

for i in "${npminstallations[@]}"
do (npm install -g "$i"); done

# Update version managers
fish -c "omf update"

