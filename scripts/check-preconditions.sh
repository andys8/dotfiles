#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
  alacritty
  bash
  fish
  i3
  i3-msg
  i3-nagbar
  i3-sensible-terminal
  i3lock-fancy
  idea
  mdcat
  mdp
  nix-channel
  nix-env
  nix-shell
  nm-applet
  nmt
  pgrep
  preload
  pulseaudio
  python3
  setxkbmap
  st
  stack
  toilet
  vim
  vimdiff
  xclip
  xrandr
  xset
  xss-lock
  zathura
)

files=(
  ~/.gitconfig.user
  ~/bin/startup.sh
  ~/Pictures/wallpaper/wallpaper.png
  /usr/local/share/lombok/lombok-1.18.6.jar
)

check() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "'$1' missing"
    exit 1
  }
}

exists() {
  if [ ! -f "$1" ]; then
    echo "'$1' missing"
    exit 1
  fi
}

# Check commands
for i in "${commands[@]}"; do check "$i"; done

# Verify files exist
for i in "${files[@]}"; do exists "$i"; done

# Otherwise good case
echo "All dependencies are available"
exit 0
