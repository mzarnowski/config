dir="$(dirname "$(readlink -f $0)")"
dotfiles="${dir}/dotfiles"

ln -s "${dotfiles}"/.yaourtrc       ~/.yaourtrc

./install_from_community.sh
./install_from_aur.sh

./dotfiles/install.sh
