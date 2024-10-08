#!/bin/bash
set -euo pipefail

# Asserted commands (should be installed by script)
assertCommands=(
    cabal                           # haskell tool
    dunstctl                        # dunst control
    elm                             # elm compiler
    elm-json                        # elm installation helper
    elm-test                        # elm test runner
    ffmpeg                          # convert videos (used in scripts)
    ghc                             # haskell compiler
    git-brunch                      # branch change tool
    haskell-language-server-wrapper # haskell language server
    hlint                           # haskell linter
    htop                            # htop (hopefully with vim bindings)
    lock                            # bin/lock file is on the path
    prettier                        # js/css/markdown formatter
    qrcode-terminal                 # show qr codes
    sgpt                            # TheR1D/shell_gpt
    st                              # suckless terminal
    stack                           # haskell tool
    tsc                             # typescript compiler
)

# Optional user commands (installed manually)
commandsOptional=(
    alacritty       # terminal-emulator
    btm             # bottom process monitor
    chromium        # chromium web browser
    idea            # intellij ide
    java            # java runtime
    json-tui        # TUI to visualize json input
    lxappearance    # configure themes
    pulseaudio      # better audio
    thorium-browser # web browser
    trash-put       # trash-cli
    uuidgen         # generate uuid
    vimdiff         # git diffs in vim
    xkill           # click on window to kill
    zathura         # pdf viewer
)

errors=0
warnings=0

warn() {
    echo "$(tput setaf 3)WARNING:$(tput sgr 0) $1"
    warnings=$((warnings + 1))
}

fail() {
    echo "$(tput setaf 1)ERROR:$(tput sgr 0) $1"
    errors=$((errors + 1))
}

commandExists() {
    command -v "$1" >/dev/null 2>&1 || {
        fail "Command '$1' missing"
    }
}

warnIfCommandMissing() {
    command -v "$1" >/dev/null 2>&1 || {
        warn "Command '$1' missing"
    }
}

# Check commands
for i in "${assertCommands[@]}"; do commandExists "$i"; done

# autorandr config
[[ $(autorandr --dry-run) == "" ]] && {
    fail "autorandr needs to be setup with configurations"
}

# preload
[[ $(systemctl is-active preload) != "active" ]] && {
    fail "service 'preload' not active (systemctl enable preload)"
}

[ -z "${JAVA_HOME-}" ] && {
    fail "\$JAVA_HOME has to be set"
}

[[ "${WINIT_HIDPI_FACTOR-}" != "1.0" ]] && {
    fail "\$WINIT_HIDPI_FACTOR has to be set to 1.0 for alacritty"
}

# cursor theme: xcursor-breeze
[[ $(find /usr/share/icons -type d -name "cursors") =~ "breeze" ]] || {
    fail "xcursor-breeze cursor theme is missing"
}

[[ $(cat ~/.config/gtk-3.0/settings.ini) =~ "breeze" ]] || {
    fail "xcursor-breeze cursor theme is not configured as gtk 3.0 theme"
}

# theme: ant-dracula
[[ -d /usr/share/themes/Ant-Dracula ]] || {
    fail "Ant-Dracula theme is missing"
}

[[ $(cat ~/.config/gtk-3.0/settings.ini) =~ "Ant-Dracula" ]] || {
    fail "Ant-Dracula theme is not configured as gtk 3.0 theme"
}

[[ $(cat ~/.gtkrc-2.0) =~ "Ant-Dracula" ]] || {
    fail "Ant-Dracula theme is not configured as gtk 2.0 theme"
}

[[ $(cat ~/.config/gtk-3.0/settings.ini) =~ "Iosevka" ]] || {
    fail "Iosevka font is not configured as gtk 3.0 font"
}

[[ $(cat ~/.gtkrc-2.0) =~ "Iosevka" ]] || {
    fail "Iosevka font is not configured as gtk 2.0 font"
}

# Make sure max brightness is configured
BRIGHTNESS=$(cat /sys/class/backlight/*/brightness)
MAX_BRIGHTNESS=$(cat /sys/class/backlight/*/max_brightness)
[[ "$BRIGHTNESS" != "$MAX_BRIGHTNESS" ]] && {
    fail "Screen brightness is $BRIGHTNESS, but should be max brightness $MAX_BRIGHTNESS"
}

# Check commands
for i in "${commandsOptional[@]}"; do warnIfCommandMissing "$i"; done

# Result
[[ $warnings != 0 ]] && echo "$(tput setaf 3)Post-Conditions have $warnings warning(s)$(tput sgr 0)"
if [[ $errors != 0 ]]; then
    echo "$(tput setaf 1)Post-Conditions failed with $errors error(s)$(tput sgr 0)" && exit 1
else
    echo "$(tput setaf 2)Post-Conditions are fine$(tput sgr 0)" && exit 0
fi
