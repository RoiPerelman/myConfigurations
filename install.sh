#!/bin/bash

# neovim
cd ~ || exit 1
git clone https://github.com/RoiPerelman/myConfigurations.git

# if i don't have sudo - install sudo
su -c "command -v sudo &> /dev/null || (apt-get update && apt-get install -y sudo)"
sudo apt update && sudo apt install -yqqq cmake gettext
sudo apt update && sudo apt install -yqqq ripgrep fd-find xclip # tools we use
sudo apt update && sudo apt install python3-venv # ruff language server wants
# tsserver language server wants
sudo chown -R $USER:$USER /usr/local
curl -fsSL https://raw.githubusercontent.com/tj/n/master/bin/n | bash -s lts

mkdir ~/Applications 2>/dev/null
cd ~/Applications || exit 1
git clone https://github.com/neovim/neovim.git
cd neovim || exit 1
git checkout nightly
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install

ln -snf ~/myConfigurations/config/nvim ~/.config/nvim

# tmux
sudo apt update && apt install -yqqq tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -snf ~/myConfigurations/tmux.conf ~/.tmux.conf

# opencode
curl -fsSL https://opencode.ai/install | bash
# scp the credentials example
# scp ~/.local/share/opencode/auth.json bx59a-isr-devC:/root/.local/share/opencode/auth.json

