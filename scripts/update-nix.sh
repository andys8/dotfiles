#!/bin/bash
set -euo pipefail

# Update the channel
nix-channel --update

# Install packages
nix-env -irf ~/dotfiles/nix/packages.nix

# Collect garbage (randomly)
RAND=$((RANDOM % 20))
[ $((RAND)) -eq 0 ] && nix-collect-garbage --delete-older-than 14d
[ $((RAND)) -eq 1 ] && nix-collect-garbage -d

echo "Nix packages updated"
