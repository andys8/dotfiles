#!/bin/bash
# Update brew
set -eu

echo "Update brew"

brew update 

# Brewfile (update via `brew bundle dump`)
cd ~/dotfiles
brew bundle --no-lock

brew upgrade
