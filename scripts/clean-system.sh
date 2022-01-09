#!/bin/bash
set -euo pipefail

# Clean trash directory
echo ">> Cleaning trash"
(cd ~ && trash-empty 30)

# Collect nix garbage (randomly)
echo ">> Cleaning nix"
RAND=$((RANDOM % 20))
[ $((RAND)) -eq 0 ] && nix-collect-garbage -d
[ ! $((RAND)) -eq 0 ] && nix-collect-garbage --delete-older-than 30d

# Yay
if [ -x "$(command -v "yay")" ]; then
    echo ">> Cleaning yay"
    yay --clean
    yay -Sc --noconfirm
fi

# Apt
if [ -x "$(command -v "apt")" ]; then
    echo ">> Cleaning apt"
    sudo apt autoremove -y
    sudo apt autoclean -y
fi

# Docker
if [ -x "$(command -v "docker")" ]; then
    echo ">> Cleaning docker (3 months)"
    docker container prune -f --filter "until=4380h"
    docker image prune -f -a --filter "until=4380h"
fi

# Yarn
if [ -x "$(command -v "yarn")" ]; then
    echo ">> Cleaning yarn"
    yarn cache clean
fi

# Autojump
echo ">> Cleaning autojump (j)"
autojump --purge

# Vim CoC
echo ">> Cleaning CoC extensions"
~/dotfiles/scripts/clean-vim-coc.sh
