#!/usr/bin/zsh

wid=$1
class=$2
instance=$3
title=$(xwininfo -id $wid | sed -n '2p' | cut -d\" -f2)

bspwm_gap=$(bspc config window_gap)
bspwm_border=$(bspc config border_width)

pos () {
    if [[ $# = 4 ]]; then
        monitor=$1
        shift
    fi

    # Store width and height of primary
    xrandr \
        | grep -P "^${monitor:-$(bspc query -M -d focused --names)}" \
        | grep -oP "\d+x\d+\+\d+\+\d+" \
        | sed 's/[x+]/ /g' \
        | read -r display_width display_height display_xoffset display_yoffset

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
    echo "${2}x$3+$((X + display_xoffset))+$((Y + display_yoffset))"
}

if [[ $class = "Emacs" ]]; then
    hash xseticon &>/dev/null &&
        xseticon -id $wid /usr/share/icons/hicolor/128x128/apps/emacs.png
elif [[ $class = "1Password" ]]; then
    hash xseticon &>/dev/null &&
        xseticon -id $wid /usr/share/icons/hicolor/64x64/apps/1password.png
fi

if [[ $instance = "gl" ]]; then
    echo $wid > /tmp/mpv-float
    echo layer=normal
elif [[ $instance = "st-256color" && $title = "htop" ]]; then
    echo state=floating
elif [[ $instance = "st-float."* ]]; then
    mon=$(echo $instance | cut -d. -f2)
    echo $wid > /tmp/$instance
    echo layer=above state=floating hidden=on sticky=on rectangle=$(pos $mon 8 1078 560)
    bspc node $wid -m $mon &
elif [[ $title = "emacs-float."* ]]; then
    mon=$(echo $title | cut -d. -f2)
    echo $wid > /tmp/$title
    echo layer=normal state=floating hidden=on sticky=on rectangle=$(pos $mon 8 1078 1110)
    bspc node $wid -m $mon &
# fix vlc control window not showing up in fullscreen
elif [[ $instance = "vlc" && $title = "vlc" ]]; then
    echo layer=above border=off
elif [[ $class = "firefox" && $title = "Picture-in-Picture" ]]; then
    echo $wid > /tmp/mpv-float
    echo state=floating sticky=on rectangle=960x540+1575+275
elif [[ $title = "Android Emulator - *" ]]; then
    echo state=floating
fi
