export UNAME=$(uname)
+linux () {[[ $UNAME == "Linux" ]] && $@}
+macos () {[[ $UNAME == "Darwin" ]] && $@}

export EDITOR='emacsclient -a nvim'
export VISUAL=$EDITOR
export RIPGREP_CONFIG_PATH=~/.config/rg/rg.conf
export NLTK_DATA=~/.local/share/nltk_data
export _JAVA_AWT_WM_NONREPARENTING=1
export DUST_HOME=~/.local/dust
export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
export LV2_PATH=/usr/lib64/lv2
export GROFF_NO_SGR=1
export KEYTIMEOUT=1

+linux source ~/.zpath
