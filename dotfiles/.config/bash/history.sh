#!/usr/bin/env bash

shopt -s histappend # Append to the history file, don't overwrite it
shopt -s cmdhist # Save multi-line commands as one command

export PROMPT_COMMAND="history -a; $PROMPT_COMMAND" # Append immediately, not after shell exits
export HISTTIMEFORMAT="[%F %T] "
export HISTCONTROL="erasedups:ignoreboth" # Avoid duplicate entries
export HISTSIZE=50000
export HISTFILESIZE=50000
