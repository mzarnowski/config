for file in ~/.config/bash/completions/*.sh ; do
    [[ -r "${file}" ]] && [[ -f "${file}" ]] && source $file;
done;

unset file;
