#!/bin/sh
# Select lastpass password with `lpass-cli`
# Based on:
# - https://github.com/taylorjoshuaw/rice-scripts/blob/master/lpass-menu.sh
# - https://github.com/Connected-Information-systems/lastpass-rofi/blob/master/lastpass-rofi.sh

# Check for required tools
REQUIRED_COMMANDS="rofi lpass zenity"
for COMMAND in $REQUIRED_COMMANDS; do
    if ! [ -x "$(command -v "$COMMAND")" ]; then
        zenity --error --text="Command '$COMMAND' not found"
        exit 1
    fi
done

# Login
if ! lpass status; then
    # environment variable `LPASS_USER` is read
    [ -z "$LPASS_USER" ] && LPASS_USER="$(zenity --entry --text='LastPass Username:')"
    [ -z "$LPASS_USER" ] && exit

    if ! lpass login --trust "$LPASS_USER"; then
        zenity --error --text="Failed to login to LastPass. Please check your credentials and try again."
        exit 1
    fi
fi

# Selection
ROFI_SELECTION="$(lpass ls --format '•%an' | grep '•' | tr '•' '\n' | sed '/^$/d' | rofi -dmenu -i -p LastPass)"
if [ -z "${ROFI_SELECTION}" ]; then
    exit
fi

# Copy to clipboard
lpass show -c --password "${ROFI_SELECTION}"
