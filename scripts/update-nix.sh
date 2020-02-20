#!/bin/bash
set -euo pipefail

# Update the channel
nix-channel --update

# all-hies with cachix
command -v "cachix" >/dev/null 2>&1 || {
  nix-env -iA cachix -f https://cachix.org/api/v1/install
}

# Install packages
nix-env -irf packages.nix

# Collect garbage (chance ~ 10%)
if [ $(( RANDOM % 10 )) -eq 0 ]; then
  nix-collect-garbage --delete-older-than 14d
fi
