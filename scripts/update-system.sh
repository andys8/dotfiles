#!/bin/bash
# Update the OS
set -eu

LINUX=$(lsb_release -i -s)
LINUX=${LINUX,,}

echo "Update system ($LINUX)"

function updateUbuntu() {
    sudo apt update
    sudo apt upgrade
}

function updateArch() {
    sudo pacman-mirrors -f 5
    yay -Syu
}

case $LINUX in
linuxmint | ubuntu) updateUbuntu ;;
manjarolinux | arch) updateArch ;;
*) echo "Unexpected linux distribution: $LINUX" && exit 1 ;;
esac
