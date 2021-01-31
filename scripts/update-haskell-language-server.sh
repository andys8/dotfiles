#!/bin/bash
set -euo pipefail

REPO=haskell/haskell-language-server
# Rev can be commit or version "x.x.x"
REV=6b6c405d14a29ab3d2e7dbb4e2f79229758d26ba

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
FOLDER=haskell-language-server-$RANDOM
echo ">> Installing haskell-language-server (building from source)"
cd /tmp
git clone --recurse-submodules https://github.com/$REPO "$FOLDER"
cd "$FOLDER"
git checkout $REV
git submodule update
stack ./install.hs hls-8.8.3 hls-8.8.4
echo ">> haskell-language-server installed with stack"
exit 0
