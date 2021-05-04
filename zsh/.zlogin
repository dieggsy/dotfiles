if command -v gpg2 &>/dev/null; then
    ! pgrep gpg-agent &> /dev/null && gpgconf --launch gpg-agent
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    gpg-connect-agent updatestartuptty /bye &> /dev/null
fi

hash dust &>/dev/null && eval $(dust env)
