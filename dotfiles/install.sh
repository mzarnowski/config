dotfiles="$(dirname "$(readlink -f $0)")"

ln -s dotfiles/.config/i3 ~/.config/i3
ln -s dotfiles/.config/ranger ~/.config/ranger
ln -s dotfiles/.config/dunst ~/.config/dunst
ln -s dotfiles/.config/chrome-flags.conf ~/.config/chrome-flags.conf


ln -s dotfiles/.i3blocks.conf ~/.i3blocks.conf
ln -s dotfiles/.Xresources ~/.Xresources
ln -s dotfiles/.zshrc ~/.zshrc
ln -s dotfiles/.vimhrc ~/.vimhrc
ln -s dotfiles/.gitconfig ~/.gitconfig
