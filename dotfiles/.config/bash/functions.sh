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

backup(){
    local target=$1
    mv "${target}"{,.bak}
}

restore(){
    local target=$1
    mv "${target}"{.bak,}
}
