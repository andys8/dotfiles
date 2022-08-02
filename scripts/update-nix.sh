#!/bin/bash
set -euo pipefail

# Update the channel
echo "Update channel"
nix-channel --update

# Install packages
echo "Update packages"
nix-env -irf ~/dotfiles/nix/packages.nix

echo "Nix updated"
