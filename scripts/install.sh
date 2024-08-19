#!/bin/sh
set -eu pipefail

section() {
    echo "$(tput setaf 4)>>=$(tput setaf 5) $1 $(tput setaf 4)=<<$(tput sgr 0)"
}

cd ~/dotfiles/scripts

section "Update dotfiles repository"
./update-dotfiles.sh

section "Check pre-conditions"
./check-preconditions.sh

section "Machine specific files"
./create-machine-specific-files.sh

section "Create symlinks"
./link.sh

section "Update Nix"
./update-nix.sh

section "Update Vim"
./update-vim.sh

section "Update Fish"
./update-fish.fish

section "Update Node/NPM"
./update-npm.sh

section "Update Xmonad"
./update-xmonad.sh

section "Update Haskell"
./update-haskell.sh

section "Update Ranger"
./update-ranger.sh

section "Run all checks"
./check-preconditions.sh
./check-postconditions.sh
