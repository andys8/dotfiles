#!/bin/bash
set -euo pipefail

# Dependencies
commands=(
	awk # used in shell scripts
	bash # shell
	fc-list # list fonts
	find # used in scripts
	fish # shell
	i3lock # lockscreen
	lightdm # Login display manager
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
	tar
	tr # used in shell scripts
	vim
	w3m # used in explainshell script
	xclip # copy/paste
	xdotool # control x via command line
	xrandr # monitor setup
	xrdb # load Xresources
	xset
	xss-lock # lock the screen
)

fonts=(
	"Font Awesome 5 Free Solid"
	"Iosevka Nerd"
	"Noto Color Emoji"
	"SauceCodePro Nerd"
)

errors=0
warnings=0

fail() {
	echo "$(tput setaf 1)ERROR:$(tput sgr 0) $1"
	errors=$((errors + 1))
}

commandExists() {
	command -v "$1" >/dev/null 2>&1 || {
		fail "Command '$1' missing"
	}
}

# Check commands
for i in "${commands[@]}"; do commandExists "$i"; done

# Vim copy paste
[[ $(vim --version) =~ \+clipboard ]] || {
	fail "Vim should have '+clipboard'"
}

# sdkman
[[ -f ~/.sdkman/bin/sdkman-init.sh ]] || {
	fail "sdkman installation is missing"
}

# qutebrowser fixed version
QUTEBROWSER_VERSION=$(qutebrowser --version | head -n1)
[[ $QUTEBROWSER_VERSION =~ v1\.1[0-9]\. ]] || {
	fail "Qutebrowser version not expected ($QUTEBROWSER_VERSION)"
}

# Check fonts
fontInstalled() {
	fc-list | grep -i "$1" >/dev/null 2>&1 || {
		fail "Font '$1' missing"
	}
}

for i in "${fonts[@]}"; do fontInstalled "$i"; done

# Result
[[ $warnings != 0 ]] && echo "$(tput setaf 2)Pre-Conditions have $warnings warning(s)$(tput sgr 0)"
if [[ $errors != 0 ]]; then
	echo "$(tput setaf 1)Pre-Conditions failed with $errors error(s)$(tput sgr 0)" && exit 1
else
	echo "$(tput setaf 2)Pre-Conditions are fine$(tput sgr 0)" && exit 0
fi
