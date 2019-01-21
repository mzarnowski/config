take(){
    local TARGET_PATH=$1
    mkdir -p "${TARGET_PATH}" && cd "${TARGET_PATH}"
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
