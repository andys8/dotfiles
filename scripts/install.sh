#!/bin/sh
set -eu pipefail

section() {
    echo "$(tput setaf 4)>>=$(tput setaf 5) $1 $(tput setaf 4)=<<$(tput sgr 0)"
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
section "Update qutebrowser"
~/dotfiles/scripts/update-qutebrowser.sh
section "Update stack"
~/dotfiles/scripts/update-stack.sh
section "Update haskell-language-server"
~/dotfiles/scripts/update-haskell-language-server.sh
section "Update ranger plugins"
~/dotfiles/scripts/update-ranger.sh
section "Update htop with vim bindings"
~/dotfiles/scripts/update-htop-vim.sh
section "Update apply-refact"
~/dotfiles/scripts/update-apply-refact.sh

section "Run all checks"
~/dotfiles/scripts/check-preconditions.sh
~/dotfiles/scripts/check-postconditions.sh
