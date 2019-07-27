#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
  @elm-tooling/elm-language-server@1.1.1
  markdownlint-cli
  prettier
  typescript
)

for i in "${npminstallations[@]}"; do (npm install -g "$i")
done
