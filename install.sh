dir="$(dirname "$(readlink -f $0)")"
dotfiles="${dir}/dotfiles"

[ -e "${HOME}/bin" ] || ln -s "${dir}/bin" "${HOME}/bin"

./dotfiles/install.sh
