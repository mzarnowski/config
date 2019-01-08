take(){
    local TARGET_PATH=$1
    mkdir -p "${TARGET_PATH}" && cd "${TARGET_PATH}"
}
