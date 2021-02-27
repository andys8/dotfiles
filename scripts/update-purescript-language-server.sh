#!/bin/bash
set -euo pipefail

REPO=andys8/purescript-language-server
# Rev can be commit or version "x.x.x"
REV=andys8-fork
FOLDER="$HOME/.cache/purescript-language-server-install"

# Installation by building from source
echo ">> Installing purescript-language-server (building from source)"
if [ -d "$FOLDER" ]; then
    echo ">> Reusing existing directory $FOLDER"
else
    git clone https://github.com/$REPO "$FOLDER"
fi

cd "$FOLDER"
git fetch
git checkout $REV || true
git pull || true
npm install
npm link
echo ">> purescript-language-server ($REV) installed with npm"
exit 0
