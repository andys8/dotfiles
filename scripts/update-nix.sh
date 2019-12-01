#!/bin/bash
set -euo pipefail

# all-hies with cachix
command -v "cachix" >/dev/null 2>&1 || {
  nix-env -iA cachix -f https://cachix.org/api/v1/install
}
cachix use all-hies

# Nix
nix-channel --update
nix-env -irf packages.nix

# Collect garbage (chance ~ 10%)
if [ $(( RANDOM % 10 )) -eq 0 ]; then
  nix-collect-garbage --delete-older-than 14d
fi
