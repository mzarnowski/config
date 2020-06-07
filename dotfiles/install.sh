#!/bin/bash

dotdir=$(dirname "$(readlink -f "$0")")

copy_dotfile()(
    local dotfile=$1
    [ "install.sh"  = "${dotfile}" ] && return 0
    [ ".config"     = "${dotfile}" ] && return 0
    [ ".ssh"        = "${dotfile}" ] && return 0
    
    local target="${HOME}/${dotfile}"
    [ -e "${target}" ]                      && 
        echo "already exists: ${dotfile}"   && 
        return 0                            
    
    ln -s "${dotdir}/${dotfile}" "${target}"
)

copy_dotfiles()(
    shopt -s dotglob

    for dotfile in "${dotdir}"/{*,.config/*}
    do
        copy_dotfile "${dotfile#${dotdir}/}"
    done
)

copy_dotfiles
