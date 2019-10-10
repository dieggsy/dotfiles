if [[ "$PREFIX" != "/data/data/com.termux"* ]]; then
    PREFIX=/usr
fi

export EDITOR='emacsclient -a nvim'
export VISUAL=$EDITOR

pathadd () {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1${PATH:+":$PATH"}"
    fi
}

pathadd $HOME/.local/bin
# pathadd $HOME/.pyenv/bin
# pathadd $HOME/.pyenv/shims
pathadd $HOME/bin
pathadd $PREFIX/lib/ccache/bin

export PATH
export KEYTIMEOUT=1
