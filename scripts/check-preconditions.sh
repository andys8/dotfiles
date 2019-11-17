#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
  alacritty # terminal-emulator
  bash # shell
  fc-list # list fonts
  fish # shell
  i3 # tiling window manager
  i3-msg
  i3-nagbar
  i3-sensible-terminal
  i3lock-fancy
  idea # intellij ide
  lynis # system audit
  mdcat # markdown viewer
  metals-vim # scala language server, nix: <https://github.com/NixOS/nixpkgs/pull/60336>
  nix-channel
  nix-env
  nix-shell
  nm-applet # network manager applet
  notify-send # notifications used by script
  pgrep # grep processes
  pip3 # python packages
  preload # keep programs in ram
  pulseaudio
  python3
  python3
  setxkbmap
  st # simple terminal
  stack # haskell build tool
  vim
  vimdiff
  xclip
  xdotool # control x via command line
  xrandr # monitor setup
  xset
  xss-lock # lock the screen
  zathura # pdf viewer
)

files=(
  ~/.gitconfig.user
  ~/bin/startup.sh
  ~/Pictures/wallpaper/wallpaper.png
  /usr/local/share/lombok/lombok-1.18.6.jar # lombok in vim
)

fonts=(
  FiraCode Nerd
  Iosevka Nerd
  SauceCodePro Nerd
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

fontInstalled() {
  fc-list | grep -i "$1" >/dev/null 2>&1 || {
    echo "Font '$1' missing"
    exit 1
  }
}

# Check commands
for i in "${commands[@]}"; do check "$i"; done

# Verify files exist
for i in "${files[@]}"; do exists "$i"; done

# Check fonts
for i in "${fonts[@]}"; do fontInstalled "$i"; done

# Otherwise good case
echo "All preconditions are fine"
exit 0
