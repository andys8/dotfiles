#!/bin/sh
set -eu

cd ~/dotfiles

echo "Format shell scripts"
shfmt -w .

echo "Format markdown, yaml and json"
prettier --write "**/*.{md,json,yaml,yml}"

echo "Format nix"
nixpkgs-fmt .
