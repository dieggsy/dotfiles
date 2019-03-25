#!/bin/sh
interface=$1
status=$2

if [[ "$status" = "up" ]] && [[ "$interface" != "tun0" ]] ; then
    echo "UPDATING TIMEZONE"
    /usr/bin/tzupdate
    # if nmcli -t dev | grep -qs 'wlp3s0:.*:Cthulhu'; then
    #     echo "CONNECTING TO VPN"
    #     nmcli con up uuid 0857dd14-47c1-4ff8-a444-94cccaa139a8
    # fi
# elif [[ "$status" = "down" ]]; then
#     echo "DISCONNECTING FROM VPN"
#     nmcli con down uuid 0857dd14-47c1-4ff8-a444-94cccaa139a8
fi
