#!/bin/bash
set -euo pipefail

# all-hies
cachix use all-hies

# Nix
nix-channel --update
nix-env -irf packages.nix

# Collect garbage
nix-collect-garbage --delete-older-than 14d
