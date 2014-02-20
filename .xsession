#!/usr/bin/env bash

xmodmap ~/.xmodmap
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

autocutsel -fork &
autocutsel -selection PRIMARY -fork &

/usr/bin/udisks --mount /dev/sda1
/usr/bin/udisks --mount /dev/sda2

# start urxvt daemon
urxvtc
if [ $? -eq 2 ] ; then
    urxvtd -q -f -o
else
    pkill urxvtc
fi

# start mpd
if [ ! -f ~/.mpd/pid ] ; then
    mpd
fi

feh --bg-scale '/home/matus/pictures/haskell.png'
#xrandr --output VGA-1 --auto --left-of LVDS-1 --output LVDS-1 --auto // bez nvidia driverov
#xrandr --output VGA-0 --auto --right-of LVDS-0 --output LVDS-0 --auto # home
#xrandr --output VGA-0 --auto --left-of LVDS-0 --output LVDS-0 --auto # brno
#xmodmap -e "pointer = 3 2 1 4 5" # seems to introduce weird bugs, but maybe it was shitty mouse instead
xinput set-button-map 10 3 2 1 4 5 6 7 8 9 10 11 12 13
xinput set-button-map 12 3 2 1 4 5 6 7 8 9 10 11 12 13
xinput set-prop 12 "Device Enabled" 0

#exec /usr/local/bin/stumpwm
exec /usr/bin/xmonad
