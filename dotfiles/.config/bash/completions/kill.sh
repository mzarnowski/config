__fuzzy_complete_kill() {
  local pattern=${COMP_WORDS[COMP_CWORD]}
  # do not attempt completion if we're specifying an option
  [[ "$pattern" == -* ]] && return 0
  local reply=$(
    FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COMPLETION_OPTS --height 50% --min-height 15 --reverse --preview 'echo {}' --preview-window down:3:wrap"
    command ps -ef | sed 1d | fzf -m -q "${pattern}" | awk '{print $2}' | tr '\n' ' '
  )
  printf '\e[5n'
  COMPREPLY=( $reply )
}

complete -F __fuzzy_complete_kill -o nospace -o default -o bashdefault kill

