# -*-sh-*-
# shellcheck shell=bash

alias MAC='[ "$(uname -s)" = "Darwin" ]'
alias LINUX='[ "$(uname -s)" = "Linux" ]'

# ----------
# package management
# ----------
BREW_ROOT="/opt/homebrew"
LINUX && BREW_ROOT="/home/linuxbrew/.linuxbrew"
[ -f "$BREW_ROOT/bin/brew" ] && eval $($BREW_ROOT/bin/brew shellenv)

# ----------
# builtins
# ----------
shopt -s histappend

export BASH_SILENCE_DEPRECATION_WARNING=1
export HISTSIZE=5000

export EDITOR="vim"
export VISUAL="vim"
if command -v emacs &>/dev/null; then
    export EDITOR="emacs -nw"
    export VISUAL="emacs -nw"
fi

alias e='$EDITOR'
alias g="grep --color=auto -insH"
alias gr="g -R"
alias l="ls -lA --color=always"
alias pg="pgrep -fal"
alias pk="pkill -f"
alias r='source $HOME/.bashrc'

# ----------
# fzf TODO
# ----------

# ----------
# git
# ----------
if command -v git &>/dev/null; then
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
fi

# ----------
# prompt
# ----------
BLUE_FG="\[\e[0;34m\]"
RED_FG="\[\e[31m\]"
YELLOW_FG="\[\e[33m\]"
ENDCOLOR="\[\e[0m\]"
GREEN_FG="\[\e[32m\]"
DEFAULT_FG="$BLUE_FG"

__active_git_branch()
{
    if ! git branch &>/dev/null; then
        return
    fi

    # yellow if current git branch is dirty
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
    PS1+="$DEFAULT_FG:/\w >> "
    PS1+="$ENDCOLOR"
}

PROMPT_COMMAND=__prompt

# ----------
# tmux
# ----------
if command -v tmux &>/dev/null; then
    alias tma="tmux attach"
    alias tmks="tmux kill-session -t"
    alias tmls="tmux list-sessions"
    alias tmns="tmux new-session -As"
fi

# ----------
# transient aliases
# ----------
TRANSIENT_CONFIG="$HOME/.config/bash/transient.sh"
[ -f "$TRANSIENT_CONFIG" ] && . "$TRANSIENT_CONFIG"

alias tacl="> $TRANSIENT_CONFIG"
alias tae="$EDITOR $TRANSIENT_CONFIG"
alias tal="cat $TRANSIENT_CONFIG"

ta() {
    echo "alias $1=\"$2\"" >> "$TRANSIENT_CONFIG"
    . "$TRANSIENT_CONFIG"
}

tacd() {
    echo "alias $1=\"cd $(pwd)\"" >> "$TRANSIENT_CONFIG"
    . "$TRANSIENT_CONFIG"
}

# ----------
# machine local config - source last to allow for overrides
# ----------
for machine_module in "$HOME/.config/bash/"*; do
    [ -f "$machine_module" ] && . "$machine_module"
done
