#!/bin/bash
set -euo pipefail

# all-hies
cachix use all-hies

# Nix
nix-channel --update
nix-env -irf manifest.nix

