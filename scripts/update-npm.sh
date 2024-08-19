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
    http-server                       # spin up http server in directory
    http-status-cli                   # http status lookup tool
    n                                 # node version manager
    npm-check-updates                 # npm update depndencies
    pnpm@7                            # alternative to npm
    prettier                          # JS/TS/CSS/Markdown formatter
    psvm-ps                           # purescript version manager
    purescript                        # purescript compiler
    purescript-language-server        # purescript ls
    purs-tidy                         # purescript formatter
    qrcode-terminal                   # Encode text as QR code
    spago                             # purescript bundler
    typescript-strict-plugin          # tsc-strict
    typescript@5.2.2                  # TS compiler
    write-good                        # check and improve written text
    yarn@1.22                         # alternative to npm
)

npm_config_loglevel=error npm install -g "${packages[@]}"
echo "NPM packages updated"
