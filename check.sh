#!/usr/bin/env bash

# Dependencies
commands=(
  alacritty
  bash
  brittany
  diff-so-fancy
  fd
  feh
  fish
  ghc-mod
  gopass
  hdevtools
  hlint
  i3
  i3-dmenu-desktop
  i3-msg
  i3-nagbar
  i3-sensible-terminal
  i3lock
  maim
  markdownlint
  ncdu
  nm-applet
  preload
  python3
  ranger
  rg
  rofi
  shellcheck
  sxiv
  toilet
  vim
  vimdiff
  xrandr
  zathura
)

files=(
  ~/.gitconfig.user
  ~/bin/startup.sh
  ~/Pictures/wallpaper/wallpaper.png
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
