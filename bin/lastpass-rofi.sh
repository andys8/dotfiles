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
    ZENITY_INPUT="$(zenity --entry --text='Please enter your LastPass username:')"
    [ -z "$ZENITY_INPUT" ] && exit

    if ! lpass login --trust "$ZENITY_INPUT"; then
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
