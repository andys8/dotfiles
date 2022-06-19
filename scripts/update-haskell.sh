#!/bin/bash
set -euo pipefail
# Install haskell-language-server via ghcup

echo ">> Installing cabal"
ghcup install cabal latest
cabal update

echo ">> Installing stack"
ghcup install stack latest

echo ">> Installing ghc"
ghcup install ghc 9.2.3
ghcup install ghc 8.8.4
ghcup install ghc 8.10.7 --set

echo ">> Checking haskell-language-server"
hlsVersion="1.7.0.0"

[[ $(haskell-language-server-wrapper --numeric-version) = "$hlsVersion" ]] || {
    echo ">> Installing haskell-language-server"
    ghcup compile hls \
        -v $hlsVersion \
        --ghc 8.10.7 \
        --ghc 8.8.4 \
        -- --ghc-options='-dynamic'
}

echo ">> Randomly checking hoogle"
if [ $((RANDOM % 10)) -eq 0 ]; then
    echo ">> Update hoogle"
    hoogle generate
fi
