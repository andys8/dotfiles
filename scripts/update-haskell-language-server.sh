#!/bin/bash
set -euo pipefail
# Install haskell-language-server via ghcup

echo ">> Installing cabal"
ghcup install cabal latest

echo ">> Installing stack"
ghcup install stack latest

echo ">> Installing ghc"
ghcup install ghc 8.8.4
ghcup install ghc 8.10.7 --set

echo ">> Installing haskell-language-server"
ghcup compile hls \
    -v 1.6.0.0 \
    --ghc 8.10.7 \
    --ghc 8.8.4

if [ $((RANDOM % 10)) -eq 0 ]; then
    echo ">> Update hoogle"
    hoogle generate
fi
