if hash gpg2 &>/dev/null; then
    gpgconf --launch gpg-agent
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    gpg-connect-agent updatestartuptty /bye >&/dev/null
fi
if [[ ! $DISPLAY && $(tty) = /dev/tty1 ]]; then
  exec startx &> /dev/null
fi
