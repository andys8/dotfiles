#!/bin/bash
set -euo pipefail

REPO=haskell/haskell-language-server
# Rev can be commit or version "x.x.x"
REV=f4c76bb6a1752087b9ad7571abb7812835bb11b8
FOLDER="$HOME/.cache/haskell-language-server-install"

command -v "haskell-language-server" >/dev/null 2>&1 && {
    VERSION=$(haskell-language-server --version)
    echo "haskell-language-server is installed"
    echo "Expected: $REV"
    echo "Actual: $VERSION"
    if [[ $VERSION =~ $REV ]]; then
        echo "-> Version is up-to-date"
        exit 0
    else
        echo "-> Version is outdated"
    fi
}

# Installation by building from source
echo ">> Installing haskell-language-server (building from source)"
if [ -d "$FOLDER" ]; then
    echo ">> Reusing existing directory $FOLDER"
else
    git clone --recurse-submodules https://github.com/$REPO "$FOLDER"
fi

cd "$FOLDER"
git fetch
git checkout $REV || true
git pull || true
git submodule update
stack ./install.hs hls-8.8.3 hls-8.8.4 hls-8.10.3
echo ">> haskell-language-server ($REV) installed with stack"
exit 0
