#!/bin/bash
set -euo pipefail

# Clean trash directory
echo ">> Cleaning trash"
(cd ~ && trash-empty -f 30)

# Docker
if [ -x "$(command -v "docker")" ]; then
    echo ">> Cleaning docker (older 720h ~ 1 month)"
    docker image prune -f -a --filter "until=720h"
    docker container prune --filter "until=720h"
fi

# Yarn
if [ -x "$(command -v "yarn")" ]; then
    echo ">> Cleaning yarn"
    yarn cache clean
fi

# pnpm
if [ -x "$(command -v "pnpm")" ]; then
    echo ">> Cleaning pnpm"
    pnpm store prune
fi

# npm
if [ -x "$(command -v "npm")" ]; then
    echo ">> Cleaning npm"
    npm cache clean --force
fi

# Vim CoC
echo ">> Cleaning CoC extensions"
~/dotfiles/scripts/clean-vim-coc.sh

# Kondo cleans project cache folders
echo ">> Cleaning project caches with kondo"
kondo --all ~

# Brew cleanup
if [ -x "$(command -v "brew")" ]; then
    echo ">> Cleaning brew"
    brew cleanup
fi

# Symlinks
echo ">> Cleaning symlinks"
~/dotfiles/scripts/clean-symlinks.sh
