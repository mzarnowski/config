take(){
    local TARGET_PATH=$1
    mkdir -p "${TARGET_PATH}" && cd "${TARGET_PATH}"
}

confirm(){
    local command=$1
    local args="${@:2}"
    printf "About to execute [${CYAN}${command}${DEFAULT} $args]. Proceed? [Y]/n "
    read reply

    case $reply in
        y|Y|'') ${command} ${args} ;;
        *) printf "[${RED}${command}${DEFAULT} ${args}] cancelled\n";;
    esac
}

fkill() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

backup(){
    local target=$1
    mv "${target}"{,.bak}
}

restore(){
    local target=$1
    mv "${target}"{.bak,}
}
