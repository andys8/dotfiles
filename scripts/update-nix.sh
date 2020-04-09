#!/bin/bash
set -euo pipefail

# Update the channel
nix-channel --update

# Install packages
nix-env -irf ~/dotfiles/nix/packages.nix

# Collect garbage (chance ~ 5%)
if [ $((RANDOM % 20)) -eq 0 ]; then
	nix-collect-garbage --delete-older-than 14d
fi

echo "Nix packages updated"
