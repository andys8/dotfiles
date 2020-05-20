#!/bin/bash
set -euo pipefail

# Clean trash directory
(cd ~ && trash-empty 30)

# Collect nix garbage (randomly)
RAND=$((RANDOM % 20))
[ $((RAND)) -eq 0 ] && nix-collect-garbage --delete-older-than 14d
[ $((RAND)) -eq 1 ] && nix-collect-garbage -d

# Yay
if [ -x "$(command -v "yay")" ]; then
	yay -Sc --noconfirm
fi

# Apt
if [ -x "$(command -v "apt")" ]; then
	sudo apt -y autoremove
	sudo apt -y clean
fi
