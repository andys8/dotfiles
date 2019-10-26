#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
  markdownlint-cli
  prettier
  typescript
  @elm-tooling/elm-language-server
)

for i in "${npminstallations[@]}"; do (npm install -g "$i")
done
