#!/bin/bash
set -euo pipefail

# Npm installable dependencies
npm set prefix ~/.npm-global
npminstallations=(
	bash-language-server # bash ls
	dockerfile-language-server-nodejs # docker ls
	elm-json # Update and change elm.json files
	fx # json viewer
	prettier # JS/TS/CSS/Markdown formatter
	qrcode-terminal # Encode text as QR code
	typescript # TS compiler
	write-good # check and improve written text
)

for i in "${npminstallations[@]}"; do
	(npm install -g "$i")
done
