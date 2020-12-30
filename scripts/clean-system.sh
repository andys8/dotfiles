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
    echo ">> Cleaning docker"
    docker container prune -f --filter "until=240h"
    docker image prune -f -a --filter "until=240h"
fi
