#!/bin/bash
# Install haskell-language-server via ghcup
# Usage: update-haskell.sh [--force]
set -euo pipefail

arg="${1:-}"

echo ">> Update ghcup"
ghcup upgrade

echo ">> Installing cabal"
ghcup install cabal latest --set

echo ">> Cabal update"
(cd /tmp && cabal update)

echo ">> Installing stack"
ghcup install stack recommended --set

echo ">> Installing ghc"
ghcup install ghc 9.6.2 --no-set
ghcup install ghc 9.4.7 --no-set
ghcup install ghc 9.2.8 --set

echo ">> Checking haskell-language-server"
hlsVersion="2.2.0.0"

if [[ $(haskell-language-server-wrapper --version) != *"$hlsVersion"* ]] || [[ "$arg" == "--force" ]]; then
    echo ">> Installing haskell-language-server ($hlsVersion)"
    ghcup install hls $hlsVersion --set
fi

echo
echo ">> Randomly checking hoogle"
if [ $((RANDOM % 10)) -eq 0 ]; then
    echo ">> Update hoogle"
    hoogle generate
fi
