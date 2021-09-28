if [[ "$TERM" = "dumb" ]]; then # fix tramp
   unsetopt zle
   export PROMPT="> "
   return
fi

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

ZPLUGINDIR=~/.local/share

plug() {
    local subdir=$2
    local plugname=$(basename $1)
    if [[ -d /usr/share/$plugname ]]; then
        source /usr/share/$plugname/$subdir/*.zsh
        # for f (/usr/share/$plugname/**/*.plugin.zsh) source "$f"
    else;
        if [[ ! -d $ZPLUGINDIR/$plugname ]]; then
            git clone "$1" $ZPLUGINDIR/$plugname
        fi
        # for f ($ZPLUGINDIR/$plugname/**/*.plugin.zsh) source "$f"
        source $ZPLUGINDIR/$plugname/$subdir/*.zsh
    fi

}

plug gh:aloxaf/fzf-tab
plug gh:hlissner/zsh-autopair
[[ "$(tty)" != "/dev/tty"* ]] && plug gh:zsh-users/zsh-autosuggestions
plug gh:zsh-users/zsh-history-substring-search
plug gh:junegunn/fzf shell

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

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
alias chicken-doc='noglob chicken-doc'
alias startx='startx &>/dev/null'
alias nmr='sudo systemctl restart NetworkManager'
hash plocate &>/dev/null && alias locate=plocate
alias clear='clear -x'
alias yay=paru
alias e="emacsclient -n --alternate-editor=''"
alias ec="emacsclient -nc --alternate-editor=''"
alias et="emacsclient -t --alternate-editor=''"
alias ssh="autossh -M 0 -o 'ServerAliveInterval=15' -o 'ServerAliveCountMax=3'"

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

hash tmux &>/dev/null && [[ "$(tty)" != "/dev/tty1" ]] && [ -z $TMUX ] && { tmux attach || tmux }
