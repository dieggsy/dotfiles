#!/usr/bin/zsh

wid=$1
class=$2
instance=$3
title=$(xwininfo -id $wid | sed -n '2p' | cut -d\" -f2)

bspwm_gap=$(bspc config window_gap)
bspwm_border=$(bspc config border_width)

# Store width and height of primary
xrandr \
    | grep -P "e-?DP-?1" \
    | awk -F'[[:space:]x+]' '{print $4 " " $5}' \
    | read -r display_width display_height

pos () {
    # Takes position, W, H and creates a bspwm geometry, where position is any
    # of 0-9, arranged in a 3x3 grid on the screen
    polybar_height=47
    W=$2
    H=$3
    case $1 in
        0)
            X=15
            Y=$((polybar_height + 15))
            ;;
        1)
            X=$((display_width/2 - (W/2 - bspwm_border)))
            Y=$((polybar_height + bspwm_gap/2))
            ;;
        2)
            X=$((display_width - W - bspwm_border*2 - bspwm_gap/2))
            Y=$((polybar_height + bspwm_gap/2))
            ;;
        3)
            X=15
            Y=$((display_height/2 - (H/2 - bspwm_border)))
            ;;
        4)
            X=$((display_width/2 - (W/2 - bspwm_border)))
            Y=$((display_height/2 - (H/2 - bspwm_border)))
            ;;
        5)
            X=$((display_width - W - bspwm_border*2 - bspwm_gap/2))
            Y=$((display_height/2 - (H/2 - bspwm_border)))
            ;;
        6)
            X=15
            Y=$((display_height - H - bspwm_border*2 - bspwm_gap/2))
            ;;
        7)
            X=$((display_width/2 - (W/2 - bspwm_border)))
            Y=$((display_height - H - bspwm_border*2 - bspwm_gap/2))
            ;;
        8)
            X=$((display_width - W - bspwm_border*2 - bspwm_gap/2))
            Y=$((display_height - H - bspwm_border*2 - bspwm_gap/2))
            ;;
    esac
    echo ${2}x$3+$X+$Y
}

if [[ $class = "Emacs" ]]; then
    hash xseticon &>/dev/null &&
        xseticon -id $wid /usr/share/icons/hicolor/128x128/apps/emacs.png
fi

if [[ $instance = "gl" ]]; then
    echo $wid > /tmp/mpv-float
    echo layer=normal
elif [[ $instance = "st-256color" && $title = "htop" ]]; then
    echo state=floating
elif [[ $instance = "st-float" ]]; then
    echo $wid > /tmp/st-float
    echo layer=above state=floating hidden=on sticky=on rectangle=$(pos 8 1078 560)
elif [[ $instance = "emacs-float" ]]; then
    echo $wid > /tmp/emacs-float
    echo layer=normal state=floating hidden=on sticky=on rectangle=$(pos 8 1078 1110)
# fix vlc control window not showing up in fullscreen
elif [[ $instance = "vlc" && $title = "vlc" ]]; then
    echo layer=above border=off
elif [[ $class = "firefox" && $title = "Picture-in-Picture" ]]; then
    echo $wid > /tmp/mpv-float
    echo state=floating sticky=on rectangle=960x540+1575+275
elif [[ $title = "Android Emulator - *" ]]; then
    echo state=floating
fi
