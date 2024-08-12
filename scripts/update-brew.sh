#!/bin/bash
# Update brew
set -eu

echo "Update brew"

brew update

# Install from brewfile (created with `brew bundle dump`)
brew bundle --file ~/dotfiles/Brewfile --no-lock
brew upgrade
