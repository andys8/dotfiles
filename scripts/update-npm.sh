#!/bin/bash
set -euo pipefail

npm set prefix ~/.npm-global

# NPM installable packages
packages=(
	bash-language-server # bash ls
	dockerfile-language-server-nodejs # docker ls
	elm-json # Update and change elm.json files
	fx # json viewer
	prettier # JS/TS/CSS/Markdown formatter
	qrcode-terminal # Encode text as QR code
	typescript # TS compiler
	uglify-js # minimize js
	write-good # check and improve written text
)

npm_config_loglevel=error npm install -g "${packages[@]}"
echo "NPM packages updated"
