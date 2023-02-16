## Colors?  Used for the prompt.
#Regular text color
BLACK='\e[0;30m'
#Bold text color
BBLACK='\e[1;30m'
#background color
BGBLACK='\e[40m'
RED='\e[0;31m'
BRED='\e[1;31m'
BGRED='\e[41m'
GREEN='\e[0;32m'
BGREEN='\e[1;32m'
BGGREEN='\e[1;32m'
YELLOW='\e[0;33m'
BYELLOW='\e[1;33m'
BGYELLOW='\e[1;33m'
BLUE='\e[0;34m'
BBLUE='\e[1;34m'
BGBLUE='\e[1;34m'
MAGENTA='\e[0;35m'
BMAGENTA='\e[1;35m'
BGMAGENTA='\e[1;35m'
CYAN='\e[0;36m'
BCYAN='\e[1;36m'
BGCYAN='\e[1;36m'
WHITE='\e[0;37m'
BWHITE='\e[1;37m'
BGWHITE='\e[1;37m'
DEFAULT='\e[m'
GRAY_LIGHT='\e[0;37m'
GRAY_DARK='\e[1;30m'

GIT_REF="\$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"
TIME="\$(date +%H:%M)"

DIRECTORY="${BLUE}\$(pwd) ${BYELLOW}${GIT_REF}${DEFAULT}"
USER_AND_HOST="${GREEN}${USER}${CYAN}@${GREEN}${HOSTNAME}${DEFAULT}"

LEN_SEP=7               # length of separators
LEN_TIME=4              # hour + minutes
LEN_PWD="\$(pwd             | wc -c)"
LEN_GIT="\$(echo ${GIT_REF} | wc -c)"
LEN_USR_INFO="\$(expr ${#USER} + ${#HOSTNAME})"
LEN_DASH="\$(expr \$(tput cols) - ${LEN_TIME} - ${LEN_PWD} - ${LEN_GIT} - ${LEN_USR_INFO} - ${LEN_SEP})"

DASHES="${GRAY_LIGHT}\$(printf "%0.s-" \$(seq 1 ${LEN_DASH}))${DEFAULT}"

PROMPT_LINE_1="${TIME} ${DIRECTORY}  ${DASHES}  ${USER_AND_HOST}"
PROMPT_LINE_2="\$ "

export PS1="${PROMPT_LINE_1}\n${PROMPT_LINE_2}"

if type -P dircolors >/dev/null ; then
	if [[ -f ~/.dir_colors ]] ; then
		eval $(dircolors -b ~/.dir_colors)
	elif [[ -f /etc/DIR_COLORS ]] ; then
		eval $(dircolors -b /etc/DIR_COLORS)
	fi
fi

alias ls='ls --color=auto'
alias grep='grep --colour=auto'
alias egrep='egrep --colour=auto'
alias fgrep='fgrep --colour=auto'

shopt -s checkwinsize # check window size on re-gaining focus #65623
shopt -s expand_aliases
shopt -s histappend   # append history, instead of overwriting it #139609

export EDITOR=/usr/bin/vim

alias magit='emacsclient -a emacs -e "(magit-status \"$(git rev-parse --show-toplevel)\")"'
alias os-update='sudo apt-get update && sudo apt-get upgrade'

source_if_exists () {
  [ -r "$1" ] && . "$1"
}

colors() {
	local fgc bgc vals seq0

	printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}

source_if_exists /usr/share/bash-completion/bash_completion
