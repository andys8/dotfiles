#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
  elm-json # Update and change elm.json files
  fx # json viewer
  ramda # used in fx (needs to be installed globally)
  prettier # JS/TS/CSS/Markdown formatter
  qrcode-terminal # Encode text as QR code
  typescript # TS compiler
)

for i in "${npminstallations[@]}"; do (npm install -g "$i")
done
