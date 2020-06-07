# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o default -o nospace -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2)" scp sftp ssh

# Add case-insensitive `killall` tab completion of running apps
_complete_running_processes ()
{
	local LC_ALL='C'
	local IFS=$'\n'
	local cur=${COMP_WORDS[COMP_CWORD]}

	COMPREPLY=()

	# do not attempt completion if we're specifying an option
	[[ "$cur" == -* ]] && return 0

	# Escape dots in paths for grep
	cur=${cur//\./\\\.}

	COMPREPLY=( $(ps axc | tail -n +2 | awk '{ print $5 }' | sort -u | grep -v "^[\-\(]" | grep -i "^$cur") )
}
complete -o bashdefault -o default -o nospace -F _complete_running_processes killall

# If possible, add tab completion for many more commands
if [ -f /opt/local/share/doc/git-core/contrib/completion/git-completion.bash ]; then
	. /opt/local/share/doc/git-core/contrib/completion/git-completion.bash
fi
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
	. /opt/local/etc/profile.d/bash_completion.sh
fi
