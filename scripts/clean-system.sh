#!/bin/bash
set -euo pipefail

# Clean trash directory
echo ">> Cleaning trash"
(cd ~ && trash-empty -f 30)

# Collect nix garbage (randomly)
echo ">> Cleaning nix"
RAND=$((RANDOM % 20))
[ $((RAND)) -eq 0 ] && nix-collect-garbage -d
[ ! $((RAND)) -eq 0 ] && nix-collect-garbage --delete-older-than 30d

# Yay
if [ -x "$(command -v "yay")" ]; then
    echo ">> Cleaning yay"
    yay -Yc --noconfirm
fi

# Apt
if [ -x "$(command -v "apt")" ]; then
    echo ">> Cleaning apt"
    sudo apt autoremove -y
    sudo apt autoclean -y
fi

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

# Cargo / Rust
if [ -x "$(command -v "cargo-cache")" ]; then
    echo ">> Cleaning cargo"
    cargo-cache -a
fi

# GHCup
if [ -x "$(command -v "ghcup")" ]; then
    echo ">> Cleaning garbage collection"
    ghcup gc --ghc-old --hls-no-ghc --cache --tmpdirs
fi

# GHCup
if [ -x "$(command -v "yarn")" ]; then
    echo ">> Cleaning yarn cache"
    yarn cache clean
fi

# Vim CoC
echo ">> Cleaning CoC extensions"
~/dotfiles/scripts/clean-vim-coc.sh

# Kondo cleans project cache folders
echo ">> Cleaning project caches with kondo"
kondo --all ~

# Symlinks
echo ">> Cleaning symlinks"
~/dotfiles/scripts/clean-symlinks.sh
