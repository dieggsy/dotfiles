[[ "$TERM" = "dumb" ]] && unsetopt zle && return

eval "$(dircolors)"
export LS_COLORS="$LS_COLORS:di=94:ex=92:"

# The following lines were added by compinstall
zmodload zsh/complist
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' format '%F{yellow}--%d--%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# zstyle ':completion:*:options' list-colors '=(#b)*(-- *)=0=90'
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt '%S%p%s'
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' verbose yes
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle :compinstall filename '/home/dieggsy/.zshrc'

autoload -Uz compinit

compinit -u
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd hist_ignore_all_dups hist_ignore_space
unsetopt beep
bindkey -v
bindkey "^?" backward-delete-char
bindkey -M menuselect '^[[Z' reverse-menu-complete
# End of lines configured by zsh-newuser-install

ZPLUGINDIR=$PREFIX/share/zsh/plugins
[ -d $ZPLUGINDIR/fzf-tab-git ] && source $ZPLUGINDIR/fzf-tab-git/fzf-tab.zsh

[ -d $ZPLUGINDIR/zsh-autopair ] && source $ZPLUGINDIR/zsh-autopair/autopair.zsh

[[ "$(tty)" != "/dev/tty"* ]] && [ -d $ZPLUGINDIR/zsh-autosuggestions ] \
    && source $ZPLUGINDIR/zsh-autosuggestions/zsh-autosuggestions.zsh

if [ -d $ZPLUGINDIR/zsh-history-substring-search ]; then
    source $ZPLUGINDIR/zsh-history-substring-search/zsh-history-substring-search.zsh
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
fi

if [ -d /usr/share/fzf/ ]; then
    source /usr/share/fzf/key-bindings.zsh
    source /usr/share/fzf/completion.zsh
    export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border'
fi

maybe_host () {
    if [ $SSH_CLIENT ] || [ $SSH_TTY ]; then
        echo "%F{13}@%M%f "
    fi
}

maybe_git () {
    hash git-prompt &> /dev/null && git-prompt
}

setopt prompt_subst
export PROMPT='$(maybe_host)$(maybe_git)%F{7}%1~%f %F{209}%(!.#.>)%f '

alias ls='ls --color=auto -F'
alias csi='csi -q'
alias lsblk='lsblk -o NAME,SIZE,MOUNTPOINT'
alias chicken-doc='noglob chicken-doc'
alias startx='startx &>/dev/null'
alias nmr='sudo systemctl restart NetworkManager'
alias locate=plocate
alias clear='clear -x'
alias e="emacsclient -n --alternate-editor=''"
alias ec="emacsclient -nc --alternate-editor=''"
alias et="emacsclient -t --alternate-editor=''"

cd_list () {
    emulate -L zsh
    ls --color=auto -F
}

chpwd_functions=(${chpwd_functions[@]} "cd_list")

zle-keymap-select () {
    if [ $KEYMAP = vicmd ]; then
        # the command mode for vi
        echo -ne "\e[2 q"
    else
        # the insert mode for vi
        echo -ne "\e[6 q"
    fi
}

if [[ "$(tty)" != "/dev/tty"* ]]; then
    zle -N zle-keymap-select
    echo -ne '\e[6 q'
fi

man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[38;5;209m") \
        LESS_TERMCAP_md=$(printf "\e[38;5;209m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[48;5;8m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[38;5;12m") \
        man "$@"
}

qmpv() {
    if [ -z "$1" ]; then
        mpv --no-terminal "$(xclip -o)" &!
    else
        mpv --no-terminal "$1" &!
    fi
}

ssh() {
    [ $TMUX ] && tmux rename-window $(echo "$*" | grep -oP '(?<=@)[^ ]+' | head -1)
    autossh -M 0 -o "ServerAliveInterval=15" -o "ServerAliveCountMax=3" $@
    [ $TMUX ] && tmux set-window-option automatic-rename on
}

missing-from-group () {
    comm -23 <(pacman -Sqg "$1" | sort) <(pacman -Qqg "$1" | sort)
}

circular-deps () {
    for pkg in $(pacman -Qqd); do
        [[ -z $(comm -12 <(pactree -r $pkg -u | sort) <(pacman -Qqe | sort)) ]] && echo $pkg;
    done
}

yay () {
    local INITIAL_QUERY="${1:-.*}"
    local RG_PREFIX="paru --topdown -Ssq"
    FZF_DEFAULT_COMMAND="$RG_PREFIX '$INITIAL_QUERY'" \
                       fzf --bind "change:reload:$RG_PREFIX {q} || true" \
                       -m --preview-window wrap --preview 'paru -Si {1} | head -n4' \
                       --ansi --disabled --query "$INITIAL_QUERY" \
                       --height=50% --layout=reverse |
        xargs -ro paru -S
}

nay () {
    paru -Qqett |
        fzf -q "$1" -m --preview-window wrap --preview 'paru -Qi {1} | head -n3' |
        xargs -ro paru -Rns
}

fuck() {
    if hash thefuck &>/dev/null; then
        eval "$(thefuck --alias)" && fuck
    else
        echo "thefuck not in \$PATH"
        return 1
    fi
}

hash tmux &>/dev/null && [[ "$(tty)" != "/dev/tty1" ]] && [ -z $TMUX ] && { tmux attach || tmux }
