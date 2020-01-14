#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
  alacritty # terminal-emulator
  bash # shell
  fc-list # list fonts
  fish # shell
  git-brunch # [TMP] Remove once git-bruch 1.0.0.0 is available in nix
  hie-wrapper # haskell ide engine
  i3 # tiling window manager
  i3-msg
  i3-nagbar
  i3-sensible-terminal
  i3lock-fancy # lockscreen
  idea # intellij ide
  java
  lynis # system audit
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
  qutebrowser # Keyboard based webbrowser
  setxkbmap
  st # simple terminal
  timeshift # backup tool
  unzip
  vim
  vimdiff
  xclip
  xdotool # control x via command line
  xrandr # monitor setup
  xset
  xss-lock # lock the screen
  zathura # pdf viewer
  zip
)

files=(
  ~/.gitconfig.user
  ~/Pictures/wallpaper/wallpaper.png
  ~/bin/startup.sh
)

fonts=(
  "Font Awesome 5 Free Solid"
  "Iosevka Nerd"
  "SauceCodePro Nerd"
)

fail() {
  echo "$1"
  exit 1
}

check() {
  command -v "$1" >/dev/null 2>&1 || {
    fail "Command '$1' missing"
  }
}

exists() {
  if [ ! -f "$1" ]; then
    fail "File '$1' missing"
  fi
}

fontInstalled() {
  fc-list | grep -i "$1" >/dev/null 2>&1 || {
    fail "Font '$1' missing"
  }
}

# Check commands
for i in "${commands[@]}"; do check "$i"; done

# Vim copy paste
[[ $(vim --version) =~ \+clipboard ]] || {
  fail "Vim should have '+clipboard'";
}

# sdkman
[[ ! -f ~/.sdkman/bin/sdkman-init.sh ]] && {
  fail "sdkman installation is missing";
}

# Verify files exist
for i in "${files[@]}"; do exists "$i"; done

# Check fonts
for i in "${fonts[@]}"; do fontInstalled "$i"; done

[ -z "${JAVA_HOME-}" ] && {
  fail "\$JAVA_HOME has to be set";
}

[[ "${WINIT_HIDPI_FACTOR-}" != "1.0" ]] && {
  fail "\$WINIT_HIDPI_FACTOR has to be set to 1.0 for alacritty";
}

# Otherwise good case
echo "All preconditions are fine"
exit 0
