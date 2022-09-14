#!/bin/bash
set -euo pipefail
# Install haskell-language-server via ghcup

echo ">> Installing cabal"
ghcup install cabal latest --set

echo ">> Cabal update"
(cd /tmp && cabal update)

echo ">> Installing stack"
ghcup install stack latest --set

echo ">> Installing ghc"
ghcup install ghc 8.10.7 --no-set
ghcup install ghc 9.2.4 --set

echo ">> Checking haskell-language-server"
hlsGitRef="1.8.0.0"
hlsVersion="1.8.0.0"

[[ $(haskell-language-server-wrapper --version) == *"$hlsVersion"* ]] || {
    echo ">> Installing haskell-language-server ($hlsVersion, $hlsGitRef)"
    ghcup compile hls \
        --git-ref "$hlsGitRef" \
        --overwrite-version "$hlsVersion" \
        --ghc 8.10.7 \
        --ghc 9.2.4 \
        --set \
        -- --ghc-options='-dynamic'
}

echo ">> Randomly checking hoogle"
if [ $((RANDOM % 10)) -eq 0 ]; then
    echo ">> Update hoogle"
    hoogle generate
fi
