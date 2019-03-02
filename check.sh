#!/usr/bin/env bash

# Dependencies
commands=(
  bash
  rofi
  i3
  i3-sensible-terminal
  i3-dmenu-desktop
  i3-nagbar
  i3lock
  xrandr
  gnome-screenshot
  volumeicon
  nm-applet
  python3
  vim
  brittany
  ghc-mod
  prettier
  elm-format
  hlint
  hdevtools
  alacritty
  fish
  eslint
  rg
  gopass
  toilet
  diff-so-fancy
  vimdiff
  vim
  shellcheck
  ~/bin/startup.sh
)

files=(
  ~/.gitconfig.user
  ~/bin/startup.sh
)

check() { command -v "$1" >/dev/null 2>&1 || { echo "'$1' missing"; exit 1; } }
exists() { if [ ! -f "$1" ]; then echo "'$1' missing"; exit 1; fi }

# Check commands
for i in "${commands[@]}"
do check "$i"; done

# Verify files exist
for i in "${files[@]}"
do exists "$i"; done

# Otherwise good case
echo "All dependencies are available"
exit 0
