#!/bin/bash

# clone myConfigurations and run its install script
if ! [[ -d "$HOME/myConfigurations" ]]; then
  cd "$HOME" || exit 1
  git clone https://github.com/RoiPerelman/myConfigurations.git
  "$HOME/myConfigurations/install.sh"
fi

# if i don't have sudo - install sudo
su -c "command -v sudo &> /dev/null || (apt-get update && apt-get install -y sudo)"
su -c "command -v cmake &> /dev/null || (sudo apt update && sudo apt install -yqqq cmake)"
su -c "command -v gettext &> /dev/null || (sudo apt update && sudo apt install -yqqq gettext)"
su -c "command -v rg &> /dev/null || (sudo apt update && sudo apt install -yqqq ripgrep)"
su -c "command -v fdfind &> /dev/null || (sudo apt update && sudo apt install -yqqq fd-find)"
su -c "command -v xclip &> /dev/null || (sudo apt update && sudo apt install -yqqq xclip)"
su -c "dpkg -s python3-venv &> /dev/null || (sudo apt update && sudo apt install -yqqq python3-venv)"

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
if ! command -v "$HOME/.opencode/bin/opencode" &> /dev/null; then
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

# fzf
if ! command -v fzf &> /dev/null; then
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
fi

# zoxide
if ! command -v zoxide &> /dev/null; then
  curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | sh
fi

# link zshrc and theme
ln -snf ~/myConfigurations/zsh/roip.zsh-theme ~/.oh-my-zsh/themes/roip.zsh-theme
ln -snf ~/myConfigurations/zsh/zshrc ~/.zshrc

