__toggle_sudo__ () {
  case $READLINE_LINE in
    sudo*) READLINE_LINE="${READLINE_LINE:5}"    ;;
    *)     READLINE_LINE="sudo ${READLINE_LINE}" ;;
  esac
}

bind -x '"\e\e": __toggle_sudo__' # double escape

