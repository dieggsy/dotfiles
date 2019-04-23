# The following lines were added by compinstall
eval "$(dircolors)"

zmodload zsh/complist
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' format '%F{yellow}--%d--%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:λ*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:options' list-colors '=(#b)*(-- *)=0=90'
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*' 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*' 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*' 'm:{[:lower:]}={[:upper:]} r:|[._-]=* r:|=*'
zstyle ':completion:*' menu yes select
zstyle ':completion:*' select-prompt '%S%p%s'
zstyle ':completion:*' original true
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' verbose yes
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle :compinstall filename '/home/dieggsy/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd hist_ignore_all_dups
unsetopt beep
bindkey -v
bindkey "^?" backward-delete-char
bindkey -M menuselect '^[[Z' reverse-menu-complete
# End of lines configured by zsh-newuser-install

ZPLUGINDIR=$PREFIX/share/zsh/plugins
[ -d $ZPLUGINDIR/zsh-autopair ] && source $ZPLUGINDIR/zsh-autopair/autopair.zsh
[ -d /$ZPLUGINDIR/zsh-autosuggestions ] && source /$ZPLUGINDIR/zsh-autosuggestions/zsh-autosuggestions.zsh
[ -d /$ZPLUGINDIR/fast-syntax-highlighting ] && source /$ZPLUGINDIR/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
if [ -d /$ZPLUGINDIR/zsh-history-substring-search ]; then
    source /$ZPLUGINDIR/zsh-history-substring-search/zsh-history-substring-search.zsh
    bindkey '^[[A' history-substring-search-up
    bindkey '^[[B' history-substring-search-down
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
fi
[ -d /$ZPLUGINDIR/zsh-notify ] && source /$ZPLUGINDIR/zsh-notify/notify.plugin.zsh

maybe_host () {
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        echo "%F{13}%n@%M%f "
    fi
}

maybe_git () {
    hash git-prompt &> /dev/null && git-prompt
}

setopt prompt_subst
[ "$TERM" != "dumb" ] && export PROMPT='$(maybe_host)$(maybe_git)%F{7}%1~%f %F{209}%(!.#.λ)%f '

alias ls='ls --color=auto -F'
alias csi='csi -q'
alias lsblk='lsblk -o NAME,SIZE,MOUNTPOINT'

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
zle -N zle-keymap-select
echo -ne '\e[6 q'

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
    mpv --no-terminal "$1" &!
}

my-packages () {
    comm -23 <(pacman -Qqett | sort) <(pacman -Qgq base base-devel xorg | sort)
}

missing-from-group () {
    comm -23 <(pacman -Sqg "$1" | sort) <(pacman -Qqg "$1" | sort)
}

if hash thefuck &>/dev/null; then
  fuck() {
    eval "$(thefuck --alias)" && fuck
  }
fi
