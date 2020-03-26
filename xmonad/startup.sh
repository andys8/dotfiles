#!/bin/sh

# Load Xresources
xrdb ~/.Xresources

# System tray
killall stalonetray
stalonetray &

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

# Battery icon(s) (and notification)
if [ -z "$(pgrep cbatticon)" ]; then
	BATTERIES=$(cd /sys/class/power_supply && find BAT*)
	for BAT in $BATTERIES; do
		cbatticon -u 30 "$BAT" &
	done
fi

# Keyboard layout: German, no dead keys
setxkbmap -layout de -variant nodeadkeys -option ctrl:nocaps

# Display standbye
xset dpms 360

# Keyboard rate
xset r rate 200 80

# No bell
xset -b

# Wallpaper
nitrogen --restore

# Run startup script (which is different on each machine)
~/bin/startup.sh &
