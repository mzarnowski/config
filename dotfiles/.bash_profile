#
# ~/.bash_profile
#

for file in ~/.config/bash/*.sh ; do
    [[ -r "${file}" ]] && [[ -f "${file}" ]] && source $file;
done;

unset file;
