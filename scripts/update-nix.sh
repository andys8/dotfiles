#!/bin/bash
set -euo pipefail

# Update the channel
nix-channel --update

# cachix
command -v "cachix" >/dev/null 2>&1 || {
	nix-env -iA cachix -f https://cachix.org/api/v1/install
}

# Install packages
nix-env -irf ~/dotfiles/nix/packages.nix

# Collect garbage (chance ~ 5%)
if [ $((RANDOM % 20)) -eq 0 ]; then
	nix-collect-garbage --delete-older-than 14d
fi
