#!/bin/bash
set -euo pipefail

# Nix
nix-channel --update
nix-env -irf manifest.nix
