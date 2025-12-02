#!/bin/bash

# clone myConfigurations and run its install script
if ! [[ -d "$HOME/myConfigurations" ]]; then
  cd "$HOME" || exit 1
  git clone https://github.com/RoiPerelman/myConfigurations.git
  "$HOME/myConfigurations/install.sh"
fi

# if i don't have sudo - install sudo
su -c "command -v sudo &> /dev/null || (apt-get update && apt-get install -y sudo)"
sudo apt update && sudo apt install -yqqq cmake gettext
sudo apt update && sudo apt install -yqqq ripgrep fd-find xclip # tools we use
sudo apt update && sudo apt install python3-venv # ruff language server wants

# tsserver language server wants
if ! [[ -d "/usr/local/n" ]]; then
  sudo chown -R "$USER":"$USER" /usr/local
  curl -fsSL https://raw.githubusercontent.com/tj/n/master/bin/n | bash -s lts
fi

# neovim
if ! command -v nvim &> /dev/null; then
  mkdir ~/Applications 2>/dev/null
  cd ~/Applications || exit 1
  git clone https://github.com/neovim/neovim.git
  cd neovim || exit 1
  git checkout nightly
  make CMAKE_BUILD_TYPE=RelWithDebInfo
  sudo make install
fi
ln -snf ~/myConfigurations/config/nvim ~/.config/nvim

# tmux
if ! command -v tmux &> /dev/null; then
  sudo apt update && apt install -yqqq tmux
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
ln -snf ~/myConfigurations/tmux.conf ~/.tmux.conf

# opencode
# scp the credentials example
# scp ~/.local/share/opencode/auth.json bx59a-isr-devC:/root/.local/share/opencode/auth.json
if ! command -v opencode &> /dev/null; then
  curl -fsSL https://opencode.ai/install | bash
fi

# zshrc
if ! command -v zsh &> /dev/null; then
  sudo apt update && sudo apt install zsh
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  sudo chsh -s "$(which zsh)"
fi
# zsh plugins
export ZSH=$HOME/.oh-my-zsh
if ! [[ -d "$ZSH/custom/plugins/zsh-syntax-highlighting" ]]; then
  git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$ZSH/custom/plugins/zsh-syntax-highlighting"
fi
if ! [[ -d "$ZSH/custom/plugins/zsh-completions" ]]; then
  git clone https://github.com/zsh-users/zsh-completions.git "$ZSH/custom/plugins/zsh-completions"
fi
if ! [[ -d "$ZSH/custom/plugins/zsh-autosuggestions" ]]; then
  git clone https://github.com/zsh-users/zsh-autosuggestions.git "$ZSH/custom/plugins/zsh-autosuggestions"
fi
if ! [[ -d "$ZSH/custom/plugins/zsh-vi-mode" ]]; then
  git clone https://github.com/jeffreytse/zsh-vi-mode.git "$ZSH/custom/plugins/zsh-vi-mode"
fi
# link zshrc and theme
ln -snf ~/myConfigurations/zsh/zshrc ~/.oh-my-zsh/themes/roip.zsh-theme
ln -snf ~/myConfigurations/zshrc ~/.zshrc

