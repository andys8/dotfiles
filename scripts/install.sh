#!/bin/sh
set -eu pipefail

section() {
	echo "$(tput setaf 2)>>> $1$(tput sgr 0)"
}

section "Update dotfiles repository"
~/dotfiles/scripts/update-dotfiles.sh

section "Check pre-conditions"
~/dotfiles/scripts/check-preconditions.sh

section "Machine specific files"
~/dotfiles/scripts/create-machine-specific-files.sh

section "Create symlinks"
~/dotfiles/scripts/link.sh

section "Update Nix"
~/dotfiles/scripts/update-nix.sh
section "Update Vim"
~/dotfiles/scripts/update-vim.sh
section "Update Fish"
~/dotfiles/scripts/update-fish.fish
section "Update Fish abbreviations"
~/dotfiles/scripts/fish-abbreviations.fish
section "Update Node/NPM"
~/dotfiles/scripts/update-npm.sh
section "Update Xmonad"
~/dotfiles/scripts/update-xmonad.sh
section "Update ghcide"
~/dotfiles/scripts/update-ghcide.sh
section "Update ranger plugins"
~/dotfiles/scripts/update-ranger.sh
section "Update htop with vim bindings"
~/dotfiles/scripts/update-htop-vim.sh

section "Check post-conditions"
~/dotfiles/scripts/check-preconditions.sh
~/dotfiles/scripts/check-postconditions.sh
