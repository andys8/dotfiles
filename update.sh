#!/usr/bin/env bash

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean +qall

# Npm installable dependencies
npminstallations=(
  elm
  elm-format
  elm-lsp
  elm-test
  prettier
  typescript
)

for i in "${npminstallations[@]}"
do (npm install -g "$i"); done

