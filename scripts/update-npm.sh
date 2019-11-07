#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
  markdownlint-cli
  prettier
  typescript
)

for i in "${npminstallations[@]}"; do (npm install -g "$i")
done
