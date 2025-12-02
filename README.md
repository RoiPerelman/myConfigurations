# PREREQUISITES

## clone and ssh

```bash
git clone https://github.com/RoiPerelman/myConfigurations.git

# add ssh, reclone with it or change to it
# create an ssh key and add it to github
ssh-keygen -t ed25519 -C "roip personal key"
# use ~/.ssh/id_ed25519_personal_github add to git config sshCommand
git config core.sshCommand 'ssh -i ~/.ssh/id_ed25519_personal_github'
# if cloned using https - change origin
git remote set-url origin git@github.com:RoiPerelman/myConfigurations.git
# or for single command
GIT_SSH_COMMAND='ssh -i ~/.ssh/id_ed25519_personal' git clone git@github.com:RoiPerelman/myConfigurations.git

ln -snf ~/myConfigurations/git/config ~/.gitconfig
```

## wezterm

```bash
# install wezterm
curl -LO https://github.com/wez/wezterm/releases/download/20240203-110809-5046fc22/wezterm-20240203-110809-5046fc22.Ubuntu22.04.deb
sudo dpkg -i ./wezterm-20240203-110809-5046fc22.Ubuntu22.04.deb
rm ./wezterm-20240203-110809-5046fc22.Ubuntu22.04.deb

mkdir -p ~/.config/wezterm
ln -snf ~/myConfigurations/config/wezterm/wezterm.lua ~/.config/wezterm/wezterm.lua
```

## nvim - linux

```bash
# preqreq for mac
brew install ripgrep fd lazygit wget luarocks node
python3 -m pip install -U pip pynvim
npm install -g tree-sitter-cli
npm install -g n

# prereq linux for nvim
sudo apt update && sudo apt install cmake gettext
sudo apt update && sudo apt install ripgrep fd-find xclip
sudo apt update && sudo apt install python3-venv # ruff language server wants
# other language servers need node
sudo chown -R $USER:$USER /usr/local
curl -fsSL https://raw.githubusercontent.com/tj/n/master/bin/n | bash -s lts
npm install -g n

# from source
mkdir ~/Applications 2>/dev/null
cd ~/Applications
git clone https://github.com/neovim/neovim.git
cd neovim
git checkout nightly
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install

# link the config
ln -snf ~/myConfigurations/config/nvim ~/.config/nvim
```

## zsh

```bash
# install zsh, omz and change shell to it
sudo apt update && sudo apt install zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
sudo chsh -s $(which zsh)
# plugins
export ZSH=$HOME/.oh-my-zsh
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-completions.git $ZSH/custom/plugins/zsh-completions
git clone https://github.com/zsh-users/zsh-autosuggestions.git $ZSH/custom/plugins/zsh-autosuggestions
git clone https://github.com/jeffreytse/zsh-vi-mode.git $ZSH/custom/plugins/zsh-vi-mode
# extra tools
curl -sS https://starship.rs/install.sh | sh
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install
curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | sh

mv ~/.zshrc ~/.zshrc_bk
ln -snf ~/myConfigurations/.zshrc ~/.zshrc
mkdir -p ~/.config && ln -snf ~/myConfigurations/.config/starship.toml ~/.config/starship.toml
```

## utilities

```bash
# set caps-lock to ctrl on linux
# tweaks -> keyboard & mouse -> additional layout options -> ctrl position
sudo apt install gnome-tweaks
```

## tmux

```bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -snf ~/myConfigurations/tmux.conf ~/.tmux.conf
```

## fonts

```bash
# for Dank Mono (and other fonts) installation
cd ~/Downloads
git clone https://github.com/cancng/fonts.git
cd fonts
./install.sh
cd ../
rm -rf fonts
```
