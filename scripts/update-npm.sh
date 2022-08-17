#!/bin/bash
set -euo pipefail

# solves npm access rights
npm set prefix ~/.npm-global

# npm is installed with nix, so it won't update itself
npm set update-notifier false

# NPM installable packages
packages=(
    asciicast2gif                     # convert asciinema to gif
    bash-language-server              # bash ls
    dockerfile-language-server-nodejs # docker ls
    fx                                # json viewer
    http-server                       # spin up http server in directory
    npm-check-updates                 # npm update depndencies
    prettier                          # JS/TS/CSS/Markdown formatter
    pscid                             # purescript compiler daemon (like ghcid)
    purescript                        # purescript compiler (purs)
    purescript-language-server        # purescript ls
    purs-tidy                         # alternative purescript formatter
    purty                             # purescript formatter
    qrcode-terminal                   # Encode text as QR code
    spago                             # purescript bundler
    typescript                        # TS compiler
    uglify-js                         # minimize js
    write-good                        # check and improve written text
)

npm_config_loglevel=error npm install -g "${packages[@]}"
echo "NPM packages updated"
