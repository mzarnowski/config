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

bind_fzf(){
    echo "fzf"
    bind -x '"\C-r": __fuzzy_history__'
}

type fzf > /dev/null 2> /dev/null && bind_fzf
