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

# \A - date
# \u - username
# \h - host
# \w - working dir

GIT_REF="\$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"

PROMPT_LINE_0="\$(printf "%0.s-" \$(seq 1 \$(tput cols)))"
PROMPT_LINE_1="╭|\A|${GREEN}\u${CYAN}@${GREEN}\h${DEFAULT}| ${BLUE}\w${DEFAULT} ${BYELLOW}${GIT_REF}${DEFAULT}"
PROMPT_LINE_2="╰\$ "
export PS1="${PROMPT_LINE_0}\n${PROMPT_LINE_1}\n${PROMPT_LINE_2}"
