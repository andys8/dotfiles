#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
  @elm-tooling/elm-language-server@1.3.2
  markdownlint-cli
  prettier
  typescript
)

for i in "${npminstallations[@]}"; do (npm install -g "$i")
done
