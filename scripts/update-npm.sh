#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
  elm-json # update and change elm.json files
  prettier # js/ts/css formatter
  typescript # ts compiler
)

for i in "${npminstallations[@]}"; do (npm install -g "$i")
done
