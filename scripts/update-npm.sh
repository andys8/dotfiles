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
    @google/gemini-cli                # google gemini cli
    http-server                       # spin up http server in directory
    http-status-cli                   # http status lookup tool
    n                                 # node version manager
    npm-check-updates                 # npm update depndencies
    pnpm                              # alternative to npm
    prettier                          # JS/TS/CSS/Markdown formatter
    purescript                        # purescript compiler
    purescript-language-server        # purescript ls
    purs-tidy                         # purescript formatter
    qrcode-terminal                   # Encode text as QR code
    spago                             # purescript bundler
    typescript-strict-plugin          # tsc-strict
    typescript                        # TS compiler
    write-good                        # check and improve written text
    yarn                              # alternative to npm
)

npm_config_loglevel=error npm install -g "${packages[@]}"
echo "NPM packages updated"
