#!/usr/bin/env bash

check() {
  command -v "$1" >/dev/null 2>&1 || { echo "$1 missing"; exit 1; }
}

check bash
check rofi
check i3
check i3-sensible-terminal
check i3-dmenu-desktop
check i3-nagbar
check i3lock
check ~/.screenlayout/startup-monitor.sh
check xrandr
check gnome-screenshot
check volumeicon
check nm-applet
check python3
check vim
check brittany
check ghc-mod
check prettier
check elm-format

echo "All dependencies are available"
exit 0
