#!/bin/bash
set -euo pipefail

# Nix
nix-env -u
nix-env -irf manifest.nix

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean! +qall

# Npm installable dependencies
npminstallations=(
  elm-test
  markdownlint-cli
  prettier
  typescript
)

for i in "${npminstallations[@]}"
do (npm install -g "$i"); done

# Update version managers
asdf update
asdf plugin-update --all
fish -c "omf update"

