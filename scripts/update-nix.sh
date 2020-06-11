#!/bin/bash
set -euo pipefail

# Update the channel
echo "Update channel"
nix-channel --update

# Install packages
echo "Update packages"
nix-env -irf ~/dotfiles/nix/packages.nix

# Update search cache
echo "Update search cache"
nix search --update-cache >/dev/null

echo "Nix updated"
