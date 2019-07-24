#!/usr/bin/env bash
set -euo pipefail

# Dependencies
commands=(
  alacritty
  asdf
  autojump
  bash
  diff-so-fancy
  dtrx
  elm
  elm-format
  elm-language-server
  fd
  feh
  fish
  floskell
  fnm
  ghc-mod
  gopass
  hlint
  i3
  i3-dmenu-desktop
  i3-msg
  i3-nagbar
  i3-sensible-terminal
  i3lock-fancy
  i3status-rs
  idea
  maim
  mdp
  ncdu
  nm-applet
  preload
  python3
  ranger
  rg
  rofi
  screenkey
  shellcheck
  shfmt
  stack
  sxiv
  toilet
  vim
  vimdiff
  wcalc
  xclip
  xrandr
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
  command -v "$1" >/dev/null 2>&1 || { echo "'$1' missing"; exit 1; }
}

exists() {
  if [ ! -f "$1" ]; then echo "'$1' missing"; exit 1; fi
}

isLocalBinary() { 
  p=$(command -v "$1")
  echo "$p" | grep -qE "/home/[a-z]+/bin/elm" || { echo "$1 should be in ~/bin/$1, but is in ${p}"; exit 1; }
}

# Check commands
for i in "${commands[@]}"
do check "$i"; done

# Verify files exist
for i in "${files[@]}"
do exists "$i"; done

# Prefer local binaries, because fnm and others slow down execution
isLocalBinary elm
isLocalBinary elm-format

# Otherwise good case
echo "All dependencies are available"
exit 0
