#!/bin/bash
set -euo pipefail

REPO=haskell/haskell-language-server
# Rev can be commit or version "x.x.x"
REV=6b6c405d14a29ab3d2e7dbb4e2f79229758d26ba
FOLDER="$HOME/.cache/haskell-language-server-install"
GHC_VERSIONS=("8.8.3" "8.8.4")

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
for ghc in "${GHC_VERSIONS[@]}"; do
    echo ">> haskell-language-server for ghc $ghc"
    stack ./install.hs "hls-$ghc"
done
echo ">> haskell-language-server ($REV) installed with stack"
exit 0
