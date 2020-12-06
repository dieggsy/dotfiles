#!/bin/sh
interface=$1
status=$2

if [[ "$status" = "up" ]] && [[ "$interface" != "tun0" ]] ; then
    echo "UPDATING TIMEZONE"
    timedatectl set-timezone "$(curl --fail https://ipapi.co/timezone)"
fi
