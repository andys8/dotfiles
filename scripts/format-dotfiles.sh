#!/bin/sh
set -eu

cd ~/dotfiles

echo "Format shell scripts"
shfmt -w -i 4 .

echo "Format markdown, yaml and json"
prettier --write "**/*.{md,json,yaml,yml}"

echo "Format haskell"
fourmolu -i ./**/*.hs

echo "Format nix"
nixpkgs-fmt .
