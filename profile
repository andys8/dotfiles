#!/bin/bash
# shellcheck disable=SC1090
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        # shellcheck disable=SC1091
        source "$HOME/.bashrc"
    fi
fi

# node global installations
PATH="$HOME/.npm-global/bin:$PATH"

# purescript psvm current binary
PATH="$HOME/.psvm/current/bin:$PATH"

# Rust (~/.cargo/bin)
if [[ ":$PATH:" != *":$HOME/.cargo/bin:"* ]]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# ~/.local/bin
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

# Nix
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    # shellcheck disable=SC1091
    source "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

# ghcup
if [[ ":$PATH:" != *":$HOME/.ghcup/bin:"* ]]; then
    export PATH="$HOME/.ghcup/bin:$PATH"
fi

# ~/bin
if [[ ":$PATH:" != *":$HOME/bin:"* ]]; then
    export PATH="$HOME/bin:$PATH"
fi

# ~/dotfiles/bin
if [[ ":$PATH:" != *":$HOME/dotfiles/bin:"* ]]; then
    export PATH="$HOME/dotfiles/bin:$PATH"
fi

# ~/dotfiles/scripts
if [[ ":$PATH:" != *":$HOME/dotfiles/scripts:"* ]]; then
    export PATH="$HOME/dotfiles/scripts:$PATH"
fi

export XDG_DATA_DIRS=$HOME/.nix-profile/share:$HOME/.share:"${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

# IDEA keyboard freeze fix
# <https://youtrack.jetbrains.com/issue/IDEA-78860>
export IBUS_ENABLE_SYNC_MODE=1

# Fix xmonad java windows
export _JAVA_AWT_WM_NONREPARENTING=1

# set environment
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
export TERMINAL=st
export BROWSER=qutebrowser
export READER=zathura
export LANG=en_US.UTF-8
export XCURSOR_THEME=xcursor-breeze
export XCURSOR_PATH=/usr/share/icons:~/.icons:~/.nix-profile/share/icons/:$XCURSOR_PATH
export RANGER_DEVICONS_SEPARATOR="  "

# Source local profile if it exists
if [ -f "$HOME/.profile.machine" ]; then
    # shellcheck disable=SC1091
    source "$HOME/.profile.machine"
fi

# Used to test if this is sourced
export PROFILE_SOURCED=1
echo ".profile sourced"
