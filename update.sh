#!/usr/bin/env bash

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean +qall

# Npm installable dependencies
npminstallations=(
  antew/elm-analyse\#lsp
  elm
  elm-format
  elm-test
  prettier
  typescript
)

for i in "${npminstallations[@]}"
do (npm install -g "$i"); done

# Setup YouCompleteMe
python3 ~/.vim/plugged/youcompleteme/install.py --ts-completer --java-completer
