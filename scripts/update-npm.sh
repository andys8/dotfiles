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
    bash-language-server              # bash ls
    csv2json                          # convert csv to json
    dockerfile-language-server-nodejs # docker ls
    http-server                       # spin up http server in directory
    http-status-cli                   # http status lookup tool
    npm-check-updates                 # npm update depndencies
    pnpm@^8.10.5                      # pnpm
    prettier                          # JS/TS/CSS/Markdown formatter
    qrcode-terminal                   # Encode text as QR code
    secure-rm-cli                     # secure delete
    typescript@5.2.2                  # TS compiler
    write-good                        # check and improve written text
)

npm_config_loglevel=error npm install -g "${packages[@]}"
echo "NPM packages updated"
