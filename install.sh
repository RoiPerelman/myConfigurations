#!bin/bash

# alacritty
ln -snf ~/myConfigurations/alacritty ~/.config/alacritty

# tmux
[ ! -d "~/.tmux/plugins/tpm" ] && git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -snf ~/myConfigurations/tmux/tmux.conf ~/.tmux.conf

# git
ln -snf ~/myConfigurations/git/config ~/.gitconfig

# nvim
ln -snf ~/myConfigurations/nvim ~/.config/nvim

# zsh
ln -snf ~/myConfigurations/zsh/zshrc ~/.zshrc
