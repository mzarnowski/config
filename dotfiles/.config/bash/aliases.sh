#!/usr/bin/env bash

alias rebash="source ~/.bash_profile"   # Reload .bash_profile

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias cat="bat"
alias cp="cp -i"                        # confirm before overwriting something
alias df='df -h'                        # human-readable sizes
alias free='free -m'                    # show sizes in MB
alias h="history"
alias l="ls"
alias ls="exa -l"
alias more=less
alias open="xdg-open"
alias path='echo $PATH | tr -s ":" "\n"' # Pretty print the path


#### git
alias gco="git checkout"
alias gst="git status --short"
alias gcp="git cherry-pick"
alias gdiff="git diff"
alias glol="git lol"
