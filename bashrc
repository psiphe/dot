# -*-sh-*-
# shellcheck shell=bash

# = Misc. Shared Setup =

_os=$(uname -s | tr '[:upper:]' '[:lower:]')
alias _mac='[ "$_os" = "darwin" ]'
alias _linux='[ "$_os" = "linux" ]'

mkdir -p "$HOME/.config/bash" # assumed to exist below

# = Package Management =

_mac && brew_root="/opt/homebrew"
_linux && brew_root="/home/linuxbrew/.linuxbrew"
[ -n "$brew_root" ] && [ -f "$brew_root/bin/brew" ] && eval "$("$brew_root"/bin/brew shellenv)"

# = Builtins =

shopt -s histappend
export HISTSIZE=5000
_mac && export BASH_SILENCE_DEPRECATION_WARNING=1

export ALTERNATE_EDITOR="emacs -nw"
export EDITOR="vim" && export VISUAL="vim"
command -v emacs &>/dev/null && export EDITOR="emacsclient -nw" && export VISUAL="emacsclient -nw"

alias e='$EDITOR'
alias f="find . -type f -name"
alias g="grep -insH"
alias gr="g -R"
alias l="ls -lA --color=always"
alias pg="pgrep -fl"
alias pk="pkill -f"
alias r='source $HOME/.bashrc'

alias ga="git add"
alias gb="git branch"
alias gc="git commit"
alias gca="git commit --amend"
alias gch="git checkout"
alias gchb="git checkout -b"
alias gcm="git commit -m"
alias gd="git diff"
alias gdc="git diff --cached" # show staged diff
alias gl="git log"
alias gs="git status"

# = Prompt =

blue_fg="\[\e[34m\]"
cyan_fg="\[\e[1;36m\]"
endcolor="\[\e[0;0;0m\]"
green_fg="\[\e[1;32m\]"
red_fg="\[\e[1;31m\]"
yellow_fg="\[\e[1;33m\]"

_active_git_branch()
{
    ! git branch &>/dev/null && return
    # cyan by default, yellow if there are uncommitted changes
    display_color="$cyan_fg"
    ! git diff --exit-code &>/dev/null && display_color="$yellow_fg"
    # parse the current branch
    branch=$(git branch 2>/dev/null | sed '/^[^*]/d' | sed -r 's/[* ]+//g')
    echo "$display_color[$branch]$endcolor"
}

_active_virtualenv()
{
    [ -n "$VIRTUAL_ENV" ] && echo "$green_fg(py::${VIRTUAL_ENV##*/})$endcolor"
}

_prompt()
{
    prevexit="$?"
    # clear the previous prompt
    PS1=""
    # print non-zero exit codes
    [ $prevexit -ne 0 ] && PS1="${red_fg}[$prevexit]"
    PS1+=$(_active_git_branch)
    PS1+=$(_active_virtualenv)
    PS1+="$blue_fg \w $"
    PS1+="$endcolor "
}

PROMPT_COMMAND=_prompt # bash evaluates $PROMPT_COMMAND before each new prompt

# = Transient Environment =
# Convenience aliases & functions to modify the shell environment on the fly,
# and persist the changes across sessions.

# Used to store on-the-fly changes; effectively part of bashrc.
transient_config="$HOME/.config/bash/transient.sh"
# shellcheck source=/dev/null
# explicitly check `-f`; ignore SC1090 - can't follow non-constant source
[ -f "$transient_config" ] && . "$transient_config"

alias te='"$EDITOR" $transient_config'
alias tecl='> "$transient_config"'
alias tel='cat "$transient_config"'

# Create a new alias named $1, equal to $2
# $1: name of the alias
# $2: value of the alias
ta() {
    [ -z "$1" ] && echo "missing the name of the alias" && return
    [ -z "$2" ] && echo "missing the value of the alias" && return
    echo "alias $1=\"$2\"" >> "$transient_config"
    # shellcheck source=/dev/null
    # explicitly check `-f`; ignore SC1090 - can't follow non-constant source
    . "$transient_config"
}

# Create a new alias named $1 to cd into $(pwd)
# $1: name of the alias
tcd() {
    [ -z "$1" ] && echo "missing the name of the alias" && return 1
    ta "$1" "cd $(pwd)"
}

# = Optional Dependencies =

# == fzf ==
# A general purpose fuzzy finder; useful for creating interactive bindings
# for command line programs, or entire interfaces.
#
# e.g. `cd $(find . -type d | fzf)` - interactively `cd` into a directory

if command -v fzf &>/dev/null; then
    # shellcheck source=/dev/null
    # explicitly check `-f`; ignore SC1090 - can't follow non-constant source
    [ -f "$HOME/.fzf.bash" ] && source ~/.fzf.bash # completions

    export FZF_COMPLETION_TRIGGER='jk' # jk<TAB> will trigger fzf completion
    # - Emacs-like scroll-up & scroll down (C-v/M-v)
    # - Glitch-theme
    export FZF_DEFAULT_OPTS="\
        --border \
        --bind ctrl-v:preview-half-page-down,alt-v:preview-half-page-up \
        --header-first \
        --height=40% \
        --layout=reverse \
        --marker='> ' \
        --no-hscroll \
        --no-mouse \
        --pointer=' >' \
        --prompt='$ ' \
        --tabstop=4 \
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
    "

    # By default fzf.bash defines three keybindings:
    # - alt-c:  fuzzy cd
    # - ctrl-r: fuzzy history search
    # - ctrl-t: fuzzy file select
    #
    # Pass additional fzf options via FZF_[ALT_C|CTRL_R|CTRL_T]_OPTS.
    export FZF_ALT_C_OPTS="--header='CD'"
    export FZF_CTRL_R_OPTS="--header='BASH HISTORY'"
    export FZF_CTRL_T_OPTS="--header='FILES' --bind 'enter:become($EDITOR {})'"

    # === fuzzy builtins === #

    # Fuzzy edit.
    # $1: Directory to s
    fe() {
        local files dir
        dir=${1:-*}
        # shellcheck disable=2086
        # The default '*' is set to perform glob expansion.
        files=$(find $dir -type f | fzf --multi --select-1 --exit-0) || return $?
        # shellcheck disable=2086
        # Word splitting is required here to pass multiple arguments.
        $EDITOR $files
    }

    # Fuzzy edit a file in $HOME/.config/
    fec() {
        fe "$HOME/.config/"
    }

    # Fuzzy select an alias.
    fal() {
        local transient_alias
        transient_alias=$(alias | fzf --header='ALL ALIASES' --exit-0 --select-1) || return $?
        eval "$(echo "$transient_alias" | sed  "s/\"/'/g" | awk -F"'" '{print $2}')"
    }

    # Fuzzy grep.
    # fg() {

    #     # grep --line-buffered --color=never -r "" * | fzf
    # }

    # Fuzzy select a manpage, with a preview window.
    # TODO the preview doesn't respect the man section
    fman() {
        local manpage
        manpage=$(man -P "less -sR" -k . | \
                      fzf --ansi --preview='man ' --header='MAN PAGES' \
                          --preview="echo {} | cut -d' ' -f 1 | tr -d ')' | cut -d'(' -f 1 | xargs -r man") || return $?
        man -S "$(echo "$manpage" | cut -d' ' -f 1 | tr -d ')' | cut -d'(' -f 2)" "$(echo "$manpage" | cut -d' ' -f 1 | tr -d ')' | cut -d'(' -f 1)"
    }

    # === fuzzy transient environment === #

    # Fuzzy select a transient alias.
    ftl() {
        local transient_alias
        transient_alias=$(fzf --header='TRANSIENT ALIASES'--select-1 --exit-0 < "$transient_config") || return $?
        eval "$(echo "$transient_alias" | sed  "s/\"/'/g" | awk -F"'" '{print $2}')"
    }

    # === fuzzy git === #

    # Fuzzy git branch (checkout).
    fgb() {
        local branch
        branch=$(git branch 2>/dev/null | sed -r 's/[* ]+//g' | \
                     fzf --prompt='git checkout ' --header='GIT BRANCHES' --exit-0 \
                         --preview="git log --color=always \
                         --format='%C(auto)%h%d %s %C(black)%C(bold)%cr% C(auto)%an' {}") || return $?
        git checkout "$branch"
    }

    # Fuzzy git log, with commit diff previews.
    fgl() {
        local commit
        commit=$(git log --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr% C(auto)%an" 2>/dev/null | \
                     fzf --prompt='git checkout ' --header='GIT LOG' --exit-0 --no-sort --reverse --ansi --height=80% \
                         --preview="\
                            echo {} | grep -o '[a-f0-9]\{7\}' | head -1 | \
                            xargs -I % sh -c 'git show --color=always %'") || return $?
        # shellcheck disable=2001
        # Prefer to use portable options when possible.
        git checkout "$(echo "$commit" | sed "s/ .*//")"
    }

    # === fuzzy tmux === #

    if command -v tmux &>/dev/null; then
        # Fuzzy attach to a tmux session.
        ftma() {
            local session
            session=$(tmux list-sessions | fzf --prompt='tmux attach -t ' --header='TMUX SESSIONS' --select-1 --exit-0) || return $?
            tmux attach -t "$(echo "$session" | cut -d":" -f 1)"
        }

        # Fuzzy select tmux sessions to kill.
        ftmk() {
            local sessions
            sessions=$(tmux list-sessions | fzf --prompt='tmux kill-session -t ' --header='TMUX SESSIONS'--exit-0 --multi) || return $?
            while IFS= read -r session ; do
                tmux kill-session -t "$(echo "$session" | cut -d":" -f 1)"
            done <<< "$sessions"
        }

        alias ftml=ftma # Sometimes I type ftmls
    fi
fi

# == tmux ==

if command -v tmux &>/dev/null; then
    alias tma="tmux attach"
    alias tmk="tmux kill-session -t"
    alias tml="tmux list-sessions"
    alias tmn="tmux new-session -As"
fi

# = Local Configuration =
# Consider everything in $HOME/.config/bash as additional shell configuration.
# Source these last so that any conflicting local configuration takes
# precedence over the defaults provided here.

for local_config in "$HOME/.config/bash/"*; do
    # shellcheck source=/dev/null
    # explicitly check `-f`; ignore SC1090 - can't follow non-constant source
    [ -f "$local_config" ] && . "$local_config"
done
