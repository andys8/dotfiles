#!/bin/bash
set -euo pipefail

# Nix
nix-channel --update
nix-env -irf manifest.nix

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean! +qall

# Update version managers
fish -c "omf update"

# Npm installable dependencies
npminstallations=(
  npm
  @elm-tooling/elm-language-server@1.1.1
  markdownlint-cli
  prettier
  typescript
)

for i in "${npminstallations[@]}"
do (npm install -g "$i"); done

