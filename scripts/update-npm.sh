#!/bin/bash
set -euo pipefail

# solves npm access rights
npm set prefix ~/.npm-global

# npm is installed with nix, so it won't update itself
npm set update-notifier false

# disable audit/fund (performance improvement)
npm set audit false
npm set fund false

# NPM installable packages
packages=(
    asciicast2gif                     # convert asciinema to gif
    bash-language-server              # bash ls
    csv2json                          # convert csv to json
    dockerfile-language-server-nodejs # docker ls
    fx                                # json viewer
    http-server                       # spin up http server in directory
    http-status-cli                   # http status lookup tool
    npm-check-updates                 # npm update depndencies
    pnpm                              # alternative to npm
    prettier                          # JS/TS/CSS/Markdown formatter
    pscid                             # purescript compiler daemon (like ghcid)
    psvm-ps                           # purescript version manager
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
