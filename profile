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
	. "$HOME/.bashrc"
    fi
fi

# node global installations
PATH="$HOME/.npm-global/bin:$PATH"

# fx
export NODE_PATH="$HOME/.npm-global/lib/node_modules"

if [ $PATH != *"$HOME/.local/bin"* ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# Nix
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
    . $HOME/.nix-profile/etc/profile.d/nix.sh;
fi

export XDG_DATA_DIRS=$HOME/.nix-profile/share:$HOME/.share:"${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

# bin in home
if [ $PATH != *"$HOME/bin"* ]; then
    PATH="$HOME/bin:$PATH"
fi

# IDEA keyboard freeze fix
# <https://youtrack.jetbrains.com/issue/IDEA-78860>
export IBUS_ENABLE_SYNC_MODE=1

# Fix xmonad java windows
export _JAVA_AWT_WM_NONREPARENTING=1

# set i3-sensible-terminal
export TERMINAL=st

# Source local profile if it exists
if [ -f "$HOME/.profile.local" ]; then
    . "$HOME/.profile.local"
fi
