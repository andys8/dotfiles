#!/bin/sh
set -eu

command -v "haskell-language-server" >/dev/null 2>&1 && {
    VERSION=$(haskell-language-server --version)
    echo "haskell-language-server is already installed."
    echo "$VERSION"
    exit 0
}

REPO=haskell/haskell-language-server
REV=0.5.1
FOLDER=haskell-language-server-$(shuf -i0-10000000 -n1)

# install with stack
echo ">> Installing haskell-language-server (building from source)"
cd /tmp
git clone -b $REV --recurse-submodules --single-branch https://github.com/$REPO "$FOLDER"
cd "$FOLDER"
stack install --stack-yaml stack-8.8.3.yaml
echo ">> haskell-language-server installed with stack"
exit 0
