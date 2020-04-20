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

scrape(){
	if [ -z "$1" ]
	then
		echo "[comma separated extensions] url"
	elif [ -z "$2" ]
	then
		local root_url=$1
		wget --recursive --html-extension --page-requisites --convert-links "${root_url}"
	else
		local root_url=$1
		local comma_separated_extensions=$2
		wget -A"${comma_separated_extensions}" --recursive --html-extension --page-requisites --convert-links "${root_url}"
	fi
}
