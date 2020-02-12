#!/bin/sh
set -eu pipefail

section() {
	echo "$(tput setaf 2)>>> $1$(tput sgr 0)"
}

section "Install and update dotfiles"

section "Update repository"
./scripts/update-repository.sh

section "Check pre-conditions"
./scripts/check-preconditions.sh

section "Create symbolic links"
./scripts/link.sh

section "Update Nix"
./scripts/update-nix.sh
section "Update Vim"
./scripts/update-vim.sh
section "Update Fish"
./scripts/update-fish.fish
section "Update Fish abbreviations"
./scripts/fish-abbreviations.fish
section "Update Node/NPM"
./scripts/update-npm.sh
section "Update Xmonad"
./scripts/update-xmonad.sh

section "Check post-conditions"
./scripts/check-preconditions.sh
./scripts/check-postconditions.sh
