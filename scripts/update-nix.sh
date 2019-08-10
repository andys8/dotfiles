#!/bin/bash
set -euo pipefail

# Nix
nix-channel --update
nix-env -irf manifest.nix

# all-hies
cachix use all-hies
nix-env -iA selection --arg selector 'p: { inherit (p) ghc864 ghc865; }' -f https://github.com/infinisil/all-hies/tarball/master
