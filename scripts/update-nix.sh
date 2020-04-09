#!/bin/bash
set -euo pipefail

# Update the channel
nix-channel --update

# Install packages
nix-env -irf ~/dotfiles/nix/packages.nix

# Collect garbage (chance ~ 5%)
[ $((RANDOM % 20)) -eq 0 ] && nix-collect-garbage --delete-older-than 14d

echo "Nix packages updated"
