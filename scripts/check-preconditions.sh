#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
  alacritty # terminal-emulator
  awk # used in shell scripts
  bash # shell
  cabal # haskell tool
  fc-list # list fonts
  fish # shell
  i3 # tiling window manager
  i3-msg
  i3-nagbar
  i3-sensible-terminal
  i3lock # lockscreen
  idea # intellij ide
  java
  lxappearance # configure themes
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
  stack # haskell build tool
  timeshift # backup tool
  tr # used in shell scripts
  unzip
  vim
  vimdiff
  xclip # copy/paste
  xdotool # control x via command line
  xrandr # monitor setup
  xrdb # load Xresources
  xset
  xss-lock # lock the screen
  zathura # pdf viewer
  zip
)

files=(
  /usr/share/backgrounds/wallpaper.jpg
  ~/.gitconfig.user
  ~/bin/startup.sh
)

fonts=(
  "Font Awesome 5 Free Solid"
  "Iosevka Nerd"
  "Noto Color Emoji"
  "SauceCodePro Nerd"
)

fail() {
  echo "ERROR: $1"
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
[[ -f ~/.sdkman/bin/sdkman-init.sh ]] || {
  fail "sdkman installation is missing";
}

# cursor theme: xcursor-breeze
[[ $(find /usr/share/icons -type d -name "cursors") =~ "breeze" ]] || {
  fail "xcursor-breeze cursor theme is missing";
}

[[ $(cat ~/.config/gtk-3.0/settings.ini) =~ "breeze" ]] || {
  fail "xcursor-breeze cursor theme is not configured as gtk 3.0 theme";
}

# theme: ant-dracula
[[ -d /usr/share/themes/Ant-Dracula ]] || {
  fail "Ant-Dracula theme is missing";
}

[[ $(cat ~/.config/gtk-3.0/settings.ini) =~ "Ant-Dracula" ]] || {
  fail "Ant-Dracula theme is not configured as gtk 3.0 theme";
}

# qutebrowser fixed version
QUTEBROWSER_VERSION=$(qutebrowser --version | head -n1)
QUTEBROWSER_EXPECTED="v1.10."
[[ $QUTEBROWSER_VERSION =~ $QUTEBROWSER_EXPECTED ]] || {
  fail "Qutebrowser version has to match $QUTEBROWSER_EXPECTED ($QUTEBROWSER_VERSION)";
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
