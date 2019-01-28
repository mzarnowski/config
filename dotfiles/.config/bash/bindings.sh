for file in ~/.config/bash/bindings/*.sh ; do
    [[ -r "${file}" ]] && [[ -f "${file}" ]] && source $file;
done;

unset file;
