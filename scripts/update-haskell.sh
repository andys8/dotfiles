#!/bin/bash
set -euo pipefail
# Install haskell-language-server via ghcup

echo ">> Update ghcup"
ghcup upgrade

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
hlsVersion="1.8.0.0"

[[ $(haskell-language-server-wrapper --version) == *"$hlsVersion"* ]] || {
    echo ">> Installing haskell-language-server ($hlsVersion)"
    ghcup compile hls \
        --git-ref "$hlsVersion" \
        --git-describe-version \
        --ghc 8.10.7 \
        --ghc 9.2.4 \
        --set \
        -- --ghc-options='-dynamic' \
        --flags="-haddockComments -eval -importLens -retrie -tactic -stan -alternateNumberFormat -gadt -explicitFixity -floskell"
}

echo ">> Randomly checking hoogle"
if [ $((RANDOM % 10)) -eq 0 ]; then
    echo ">> Update hoogle"
    hoogle generate
fi
