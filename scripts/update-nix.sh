#!/bin/bash
set -euo pipefail

# all-hies with cachix
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use all-hies

# Nix
nix-channel --update
nix-env -irf packages.nix

# Collect garbage
nix-collect-garbage --delete-older-than 14d
