__fzf_select__() {
  local cmd="${FZF_CTRL_T_COMMAND:-"command find -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | cut -b3-"}"
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" fzf -m "$@" | while read -r item; do
    printf '%q ' "$item"
  done
  echo
}



__fuzzy_history__() {
  local line
  shopt -u nocaseglob nocasematch
  line=$(
    export FZF_DEFAULT_OPTS="\
        --height 40% \
        ${FZF_DEFAULT_OPTS} \
        --tac --sync -n2..,.. \
        --tiebreak=index \
        --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m"
    
    history | fzf -q "${READLINE_LINE}" | command grep '^ *[0-9]') && 
    
    READLINE_LINE=$(echo $line | awk '{sub($1 FS,"")}7')
}

__fuzzy_files__() {
    local selected="$(__fzf_select__)"
    READLINE_LINE="${READLINE_LINE:0:$READLINE_POINT}$selected${READLINE_LINE:$READLINE_POINT}"
    READLINE_POINT=$(( READLINE_POINT + ${#selected} ))
}

bind_fzf(){
    bind -x '"\C-r": __fuzzy_history__'
    bind -x '"\C-t": __fuzzy_files__'
}

[ -d ${HOME}/.fzf/bin ] && bind_fzf
