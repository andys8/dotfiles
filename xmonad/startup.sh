#!/bin/sh

# System tray
if [ -z "$(pgrep trayer)" ]; then
    trayer \
        --monitor primary \
        --edge bottom \
        --align center \
        --widthtype request \
        --heighttype pixel \
        --height 22 \
        --iconspacing 3 \
        --alpha 0 \
        --transparent true \
        --tint 0x44475a &
fi

# Network Manager Icon
if [ -z "$(pgrep nm-applet)" ]; then
    nm-applet --sm-disable &
fi

# Volume Icon
if [ -z "$(pgrep pa-applet)" ]; then
    pulseaudio -D;
    pa-applet &
fi

# Lock screen on disabled monitor
if [ -z "$(pgrep xss-lock)" ]; then
    xss-lock -l -- i3lock-fancy &
fi

# Battery warning script
if [ -z "$(pgrep -f i3-battery-popup)" ]; then
    ~/.config/i3/i3-battery-popup -n &
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
feh --bg-fill ~/Pictures/wallpaper/wallpaper.png

# Run startup script (which is different on each machine)
~/bin/startup.sh &
