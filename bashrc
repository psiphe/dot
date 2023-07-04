# -*-sh-*-
# shellcheck shell=bash
#
# Divided into sections:
# * Package Management
# * Builtins
# * Transient Environment
# * External Packages
# * Local Overrides

alias MAC='[ "$(uname -s)" = "Darwin" ]'
alias LINUX='[ "$(uname -s)" = "Linux" ]'

# 0 - Package Management

MAC && BREW_ROOT="/opt/homebrew"
LINUX && BREW_ROOT="/home/linuxbrew/.linuxbrew"
[ -f "$BREW_ROOT/bin/brew" ] && eval $($BREW_ROOT/bin/brew shellenv)

# 1 - Builtins

shopt -s histappend
MAC && export BASH_SILENCE_DEPRECATION_WARNING=1
export HISTSIZE=5000
export EDITOR="vim"
export VISUAL="vim"
if command -v emacs &>/dev/null; then
    export EDITOR="emacs"
    export VISUAL="emacs"
fi

alias e='$EDITOR'
alias f="find . -type f -name"
alias g="grep --color=auto -insH"
alias gr="g -R"
alias l="ls -lA --color=always"
alias pg="pgrep -fal"
alias pk="pkill -f"
alias r='source $HOME/.bashrc'

alias ga="git add"
alias gb="git branch"
alias gch="git checkout"
alias gchb="git checkout -b"
alias gc="git commit"
alias gca="git commit --amend"
alias gcm="git commit -m"
alias gd="git diff"
alias gdc="git diff --cached" # show staged diff
alias gl="git log"
alias gs="git status"

# dynamic prompt
BLACK_FG="\[\e[0;30m\]"
RED_FG="\[\e[31m\]"
GREEN_FG="\[\e[32m\]"
YELLOW_FG="\[\e[33m\]"
ENDCOLOR="\[\e[0m\]"
DEFAULT_FG="$BLACK_FG"

__active_git_branch()
{
    if ! git branch &>/dev/null; then
        return
    fi

    # yellow if there are uncommitted changes
    display_color="$GREEN_FG"
    if ! git diff --exit-code &>/dev/null; then
        display_color="$YELLOW_FG"
    fi

    # parse the current branch
    branch=$(git branch 2>/dev/null | sed '/^[^*]/d' | sed -r 's/[* ]+//g')
    echo "${display_color}[$branch]"
}

__prompt()
{
    prevexit="$?"

    # print non-zero error codes in red
    PS1=""
    if [ $prevexit -ne 0 ]; then
        PS1="${RED_FG}[$prevexit]"
    fi

    PS1+="$(__active_git_branch)"
    PS1+="$DEFAULT_FG:/\w $ "
    PS1+="$ENDCOLOR"
}

PROMPT_COMMAND=__prompt

# 2 - Transient Environment

TRANSIENT_CONFIG="$HOME/.config/bash/transient.sh"
[ -f "$TRANSIENT_CONFIG" ] && . "$TRANSIENT_CONFIG"
TRANSIENT_BOOKMARKS="$HOME/.transient_bookmarks"

alias tacl="> $TRANSIENT_CONFIG"
alias tbcl="> $TRANSIENT_BOOKMARKS"
alias tae="$EDITOR $TRANSIENT_CONFIG"
alias tbe="$EDITOR $TRANSIENT_BOOKMARKS"
alias tal="cat $TRANSIENT_CONFIG"
alias tbl="cat $TRANSIENT_BOOKMARKS"

# Create a new alias named $1, equal to $2
# $1: name of the alias
# $2: value of the alias
ta() {
    [ -z "$1" ] && echo "missing the name of the alias" && return
    [ -z "$2" ] && echo "missing the value of the alias" && return
    echo "alias $1=\"$2\"" >> "$TRANSIENT_CONFIG"
    . "$TRANSIENT_CONFIG"
}

# Create a new alias named $1 to cd into $(pwd)
# $1: name of the alias
tacd() {
    [ -z "$1" ] && echo "missing the name of the alias" && return
    ta $1 "cd $(pwd)"
}

# Push a new bookmark
# $1: bookmark name
# $2: url
tb() {
    [ -z "$1" ] && echo "missing the name of the bookmark" && return
    [ -z "$2" ] && echo "missing the url of the bookmark" && return
    echo "$1->$2" >> "$TRANSIENT_BOOKMARKS"
}

# 3 - External Packages

# fzf
configure_fzf() {
    [ -f ~/.fzf.bash ] && source ~/.fzf.bash # completions
    # TODO: pull the theme out
    export FZF_DEFAULT_OPTS="\
        --border \
        --bind ctrl-v:preview-half-page-down,alt-v:preview-half-page-up \
        --color=bg+:#292e42 \
        --color=bg:#24283b \
        --color=border:#414868 \
        --color=fg+:#c6d0f5 \
        --color=fg:#a9b1d6 \
        --color=header:#7892bf,header:bold \
        --color=hl+:#4c9e8a \
        --color=hl:#4c9e8a \
        --color=info:#4c9e8a \
        --color=marker:#e0af68 \
        --color=pointer:#c0caf5 \
        --color=prompt:#7aa2f7 \
        --color=spinner:#f2d5cf \
        --header-first
        --height=40% \
        --layout=reverse \
        --marker='> ' \
        --no-hscroll \
        --no-mouse \
        --pointer=' >' \
        --prompt='$ ' \
        --tabstop=4 \
    "
    export FZF_ALT_C_OPTS="--header='CD'"
    export FZF_CTRL_R_OPTS="--header='BASH HISTORY'"
    export FZF_CTRL_T_OPTS="--header='FILES' --bind 'enter:become(emacs {})'" # TODO: open in $EDITOR

    alias ftbl="cat $TRANSIENT_BOOKMARKS | fzf -m"
}
if command -v fzf &>/dev/null; then
    configure_fzf
fi

# tmux
alias tma="tmux attach"
alias tmks="tmux kill-session -t"
alias tmls="tmux list-sessions"
alias tmns="tmux new-session -As"

# 4 - Local Overrides

for machine_module in "$HOME/.config/bash/"*; do
    [ -f "$machine_module" ] && . "$machine_module"
done
