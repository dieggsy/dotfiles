if [[ "$PREFIX" != "/data/data/com.termux"* ]]; then
    PREFIX=/usr
fi

export EDITOR='emacsclient -a nvim'
export VISUAL=$EDITOR
export RIPGREP_CONFIG_PATH=~/.config/rg/rg.conf
export NLTK_DATA=~/.local/share/nltk_data
export _JAVA_AWT_WM_NONREPARENTING=1
export DUST_HOME=~/.local/dust
export GDK_DPI_SCALE=.5
export GDK_SCALE=2

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

# opam configuration
test -r /home/dieggsy/.opam/opam-init/init.zsh && . /home/dieggsy/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
