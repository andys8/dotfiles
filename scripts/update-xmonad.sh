#!/bin/bash
set -euo pipefail

xmonad --recompile
echo "XMonad recompiled"

[[ ! $(ls -1 /usr/share/xsessions/) =~ xmonad\.desktop ]] && {
	echo "Lightdm: Create xmonad entry"
	sudo cp ~/dotfiles/xmonad/xmonad.desktop /usr/share/xsessions/xmonad.desktop
}
