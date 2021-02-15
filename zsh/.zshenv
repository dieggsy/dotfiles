if [[ "$PREFIX" != "/data/data/com.termux"* ]]; then
    PREFIX=/usr
fi

export EDITOR='emacsclient -a nvim'
export VISUAL=$EDITOR
export _JAVA_AWT_WM_NONREPARENTING=1
export DUST_HOME=~/.local/dust
export GDK_DPI_SCALE=2.19

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
eval $(dust env)
