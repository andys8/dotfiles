#!/bin/bash
set -euo pipefail
# Usage: ./update-haskell-language-server.sh [-f/--force]

REPO=haskell/haskell-language-server
# Rev can be commit or version "x.x.x"
REV=1.5.1
FOLDER="$HOME/.cache/haskell-language-server-install"
FLAG=${1:-}

command -v "haskell-language-server" >/dev/null 2>&1 && {
    VERSION=$(haskell-language-server --version)
    echo "haskell-language-server is installed"
    echo "Expected: $REV"
    echo "Actual: $VERSION"
    if [[ $VERSION =~ $REV ]]; then
        echo "-> Version is up-to-date"
        if [[ $FLAG == "-f" ]] || [[ "$FLAG" == "--force" ]]; then
            echo "-> Force Update"
        else
            exit 0
        fi
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

echo "Delete previous hls binaries"
rm -v "$(stack path --local-bin)"/haskell-language-server* || true

cd "$FOLDER"
git fetch
git checkout $REV || true
git pull || true
git submodule update
stack ./install.hs "hls-8.6.5" "hls-8.8.4" "hls-8.10.7"
echo ">> haskell-language-server ($REV) installed with stack"
exit 0
