#!/bin/bash
set -euo pipefail

# Update the channel
nix-channel --update

# Install packages
nix-env -irf ~/dotfiles/nix/packages.nix

echo "Nix packages updated"
