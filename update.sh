#!/usr/bin/env bash
set -euo pipefail

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean! +qall

# Npm installable dependencies
npminstallations=(
  elm-format
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

