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
    | grep -P "e-?DP1" \
    | awk -F'[[:space:]x+]' '{print $4 " " $5}' \
    | read -r width height

bottompos=$((height - floath - border*2 - gap/2))
rightpos=$((width - floatw - border*2 - gap/2))
leftpos=$((gap/2))
bottomleft="+$leftpos+$bottompos"
bottomright="+$rightpos+$bottompos"

if [[ $instance = "gl" ]]; then
    echo $wid > /tmp/mpv-float
    echo layer=normal
elif [[ $instance = "st-256color" && $title = "htop" ]]; then
    echo state=floating
elif [[ $instance = "st-float" ]]; then
    echo $wid > /tmp/st-float
    echo layer=above state=floating hidden=on \
         sticky=on rectangle=${floatsize}${bottomright}
# fix vlc control window not showing up in fullscreen
elif [[ $instance = "vlc" && $title = "vlc" ]]; then
    echo layer=above border=off
elif [[ $class = "firefox" && $title = "Picture-in-Picture" ]]; then
    echo $wid > /tmp/mpv-float
    echo state=floating sticky=on rectangle=960x540+1575+275
elif [[ $title = "Android Emulator - *" ]]; then
    echo state=floating
elif [[ $title = "polybar-levels*"  ]]; then
    layer=above
fi
