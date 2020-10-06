#!/bin/sh

# Load Xresources
xrdb ~/.Xresources

# System tray
[ -n "$(pgrep trayer)" ] && killall trayer
trayer \
    --align center \
    --alpha 0 \
    --distance 2 \
    --edge top \
    --height 18 \
    --heighttype pixel \
    --iconspacing 6 \
    --monitor primary \
    --tint 0x282a36 \
    --transparent true \
    --widthtype request &

# Notification daemon
if [ -z "$(pgrep dunst)" ]; then
    dunst &
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

# Battery warning
if [ -z "$(pgrep -f i3-battery-popup)" ]; then
    i3-battery-popup -n &
fi

# Hide idle mouse
if [ -z "$(pgrep unclutter)" ]; then
    unclutter &
fi

# Keyboard layout: German, no dead keys
setxkbmap -layout de -variant nodeadkeys -option ctrl:nocaps

# Display standbye
xset dpms 360

# Keyboard rate
xset r rate 300 80

# No bell
xset -b

# Wallpaper
nitrogen --set-zoom-fill --random ~/Pictures/wallpaper/

# Run startup script (which is different on each machine)
~/bin/startup.sh &
