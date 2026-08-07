#!/bin/bash
set -euo pipefail

# Clean trash directory
echo ">> Cleaning trash"
(cd ~ && trash-empty -f 14)

# Docker
# Note: volumes are intentionally left alone. `docker volume prune` has no
# age-based filter, so it would risk deleting project data (e.g. dev DBs in
# developer-portal) that just isn't in active use right now. Prune volumes
# manually and deliberately if disk space is still tight after this.
if [ -x "$(command -v "docker")" ]; then
    echo ">> Cleaning docker (older 720h ~ 1 month)"
    docker image prune -f -a --filter "until=720h"
    docker container prune --filter "until=720h"
    docker builder prune -f --filter "until=720h"
fi

# Colima
# `colima prune` only clears cached downloaded VM assets, not space freed by
# the docker prune above. fstrim tells the VM's filesystem to release freed
# blocks back to the host, which is what actually shrinks the VM disk file.
if [ -x "$(command -v "colima")" ]; then
    echo ">> Cleaning colima"
    colima prune -f
    if colima status &>/dev/null; then
        echo ">> Reclaiming freed space inside colima VM"
        colima ssh -- sudo fstrim -av || true
    fi
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
kondo --all --older=7d ~

# Brew cleanup
if [ -x "$(command -v "brew")" ]; then
    echo ">> Cleaning brew"
    brew cleanup
fi

# Symlinks
echo ">> Cleaning symlinks"
~/dotfiles/scripts/clean-symlinks.sh
