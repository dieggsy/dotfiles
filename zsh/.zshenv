export PREFIX="${PREFIX:-/usr}"

export EDITOR='emacsclient -a nvim'
export VISUAL=$EDITOR
export RIPGREP_CONFIG_PATH=~/.config/rg/rg.conf
export NLTK_DATA=~/.local/share/nltk_data
export _JAVA_AWT_WM_NONREPARENTING=1
export DUST_HOME=~/.local/dust
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'

pathadd () {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1${PATH:+":$PATH"}"
    fi
}

pathadd $HOME/.local/bin
pathadd $HOME/bin

export PATH
export KEYTIMEOUT=1
