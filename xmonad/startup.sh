#!/bin/sh

# System tray
if [ -z "$(pgrep trayer)" ]; then
    trayer \
        --align center \
        --alpha 0 \
        --distance 2 \
        --edge bottom \
        --height 18 \
        --heighttype pixel \
        --iconspacing 6 \
        --monitor primary \
        --tint 0x44475a \
        --transparent true \
        --widthtype request &
fi

# Network Manager Icon
if [ -z "$(pgrep nm-applet)" ]; then
    nm-applet --sm-disable &
fi

# Volume Icon
if [ -z "$(pgrep pa-applet)" ]; then
    pa-applet &
fi

# Lock screen on disabled monitor
if [ -z "$(pgrep xss-lock)" ]; then
    xss-lock -l -- lock &
fi

# Battery icon (and notification)
if [ -z "$(pgrep cbatticon)" ]; then
    cbatticon -u 30 &
fi

# Keyboard layout: German, no dead keys
setxkbmap -layout de -variant nodeadkeys -option ctrl:nocaps

# Display standbye
xset dpms 180

# Keyboard rate
xset r rate 200 80

# No bell
xset -b

# Wallpaper
feh --bg-fill /usr/share/backgrounds/wallpaper.jpg

# Run startup script (which is different on each machine)
~/bin/startup.sh &
