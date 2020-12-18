#!/bin/bash
set -euo pipefail

REPO=haskell/haskell-language-server
REV=0.7.1

command -v "haskell-language-server" >/dev/null 2>&1 && {
    VERSION=$(haskell-language-server --numeric-version)
    echo "haskell-language-server is installed"
    echo "Expected: $REV (tag)"
    echo "Actual: $VERSION (numeric version)"
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
git clone -b $REV --recurse-submodules --single-branch https://github.com/$REPO "$FOLDER"
cd "$FOLDER"
stack ./install.hs hls-8.8.3 hls-8.8.4
echo ">> haskell-language-server installed with stack"
exit 0
