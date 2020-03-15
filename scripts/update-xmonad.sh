#!/bin/bash
set -euo pipefail

xmonad --recompile
echo "XMonad recompiled"

SOURCE=~/dotfiles/xmonad/xmonad.desktop
TARGET=/usr/share/xsessions/xmonad.desktop

if [[ "$(cat $SOURCE)" != "$(cat $TARGET 2> /dev/null || echo '')" ]]; then
	echo "Lightdm: Write xmonad entry"
	sudo cp -v $SOURCE $TARGET
fi
