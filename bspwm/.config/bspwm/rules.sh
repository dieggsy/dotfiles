#!/usr/bin/zsh

wid=$1
class=$2
instance=$3
title=$(xwininfo -id $wid | sed -n '2p' | cut -d\" -f2)

gap=$(bspc config window_gap)
border=$(bspc config border_width)
floatw=1085
floath=560
floatsize="${floatw}x${floath}"

# Store width and height of primary
xrandr \
    | grep primary \
    | awk -F'[[:space:]x+]' '{print $4 " " $5}' \
    | read -r width height

bottompos=$((height - floath - border*2 - gap/2))
rightpos=$((width - floatw - border*2 - gap/2))
leftpos=$((gap/2))
bottomleft="+$leftpos+$bottompos"
bottomright="+$rightpos+$bottompos"

if [[ $instance = "emacs" ]]; then
    echo state=tiled desktop=^2
elif [[ $instance = "erc_float" ]]; then
    echo $wid > /tmp/erc-float
    echo layer=above state=floating hidden=on \
         sticky=on rectangle=${floatsize}${bottomleft}
elif [[ $instance = "gl" ]]; then
    echo $wid > /tmp/mpv-float
elif [[ $instance = "urxvt" && $title = "htop" ]]; then
    echo state=floating
elif [[ $instance = "urxvt-float" ]]; then
    echo $wid > /tmp/urxvt-float
    echo layer=above state=floating hidden=on \
         sticky=on rectangle=${floatsize}${bottomright}
elif [[ $instance = "guvcview" ]]; then
    echo sticky=on rectangle=520x390+2015+206
elif [[ $instance = "vlc" && $title = "vlc" ]]; then
    echo layer=above border=off
fi
