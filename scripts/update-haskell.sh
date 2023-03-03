#!/bin/bash
# Install haskell-language-server via ghcup
# Usage: update-haskell.sh [--force]
set -euo pipefail

arg="${1:-}"

echo ">> Update ghcup"
ghcup upgrade

echo ">> Installing cabal"
ghcup install cabal recommended --set

echo ">> Cabal update"
(cd /tmp && cabal update)

echo ">> Installing stack"
ghcup install stack recommended --set

echo ">> Installing ghc"
ghcup install ghc 8.10.7 --no-set
ghcup install ghc 9.0.2 --no-set
ghcup install ghc 9.2.7 --set

echo ">> Checking haskell-language-server"
hlsVersion="1.9.1.0"

if [[ $(haskell-language-server-wrapper --version) != *"$hlsVersion"* ]] || [[ "$arg" == "--force" ]]; then
    echo ">> Installing haskell-language-server ($hlsVersion)"
    ghcup compile hls \
        --git-ref "$hlsVersion" \
        --git-describe-version \
        --ghc 8.10.7 \
        --ghc 9.2.7 \
        --ghc 9.0.2 \
        --set \
        -- --ghc-options='-dynamic' \
        --flags="-haddockComments -eval -importLens -retrie -tactic -stan -alternateNumberFormat -gadt -explicitFixity -floskell"
fi

echo
echo ">> Randomly checking hoogle"
if [ $((RANDOM % 10)) -eq 0 ]; then
    echo ">> Update hoogle"
    hoogle generate
fi
