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
if [[ ":$PATH:" != *":$HOME/.npm-global/bin:"* ]]; then
    export PATH="$HOME/.npm-global/bin:$PATH"
fi

# ~/.local/bin
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    export PATH="$HOME/.local/bin:$PATH"
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

export XDG_DATA_DIRS=$HOME/.share:"${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"

# n (node version manager) directory
export N_PREFIX=$HOME/.local

export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
export TERMINAL=xterm-256color
export LANG=en_US.UTF-8
export RANGER_DEVICONS_SEPARATOR="  "

# Brew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Source local profile if it exists
if [ -f "$HOME/.profile.machine" ]; then
    # shellcheck disable=SC1091
    source "$HOME/.profile.machine"
fi

# Used to test if this is sourced
export PROFILE_SOURCED=1
