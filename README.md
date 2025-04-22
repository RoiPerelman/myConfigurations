# PREREQUISITES

## clone and ssh

```bash
git clone https://github.com/RoiPerelman/myConfigurations.git
# create an ssh key and add it to github
ssh-keygen -t ed25519 -C "roip-$PC personal github"
# use ~/.ssh/id_ed25519_personal_github
git clone git@github.com:RoiPerelman/myConfigurations.git
# if cloned using https - change origin
git remote set-url origin git@github.com:RoiPerelman/myConfigurations.git
```

## wezterm

```bash
# install wezterm
curl -LO https://github.com/wez/wezterm/releases/download/20240203-110809-5046fc22/wezterm-20240203-110809-5046fc22.Ubuntu22.04.deb
sudo dpkg -i ./wezterm-20240203-110809-5046fc22.Ubuntu22.04.deb
rm ./wezterm-20240203-110809-5046fc22.Ubuntu22.04.deb

mkdir -p ~/.config/wezterm
ln -snf ~/myConfigurations/.config/wezterm/wezterm.lua ~/.config/wezterm/wezterm.lua
```

## nvim

``` bash
# prereq linux for nvim
sudo apt update && sudo apt install cmake gettext
sudo apt update && sudo apt install ripgrep fd-find xclip imagemagick gs tectonic mmdc
# ruff language server wants
sudo apt update && sudo apt install python3-venv
# other language servers need node
sudo chown $USER:$USER /usr/local
curl -fsSL https://raw.githubusercontent.com/tj/n/master/bin/n | bash -s lts
npm install -g n

mkdir ~/Applications
cd ~/Applications

# for linux tar.gz
curl -L -O https://github.com/neovim/neovim/releases/latest/download/nvim-linux64.tar.gz
tar -xvf nvim-linux64.tar.gz && rm -rf nvim-linux64.tar.gz
sudo ln -snf `realpath ./nvim-linux64/bin/nvim` /usr/local/bin/nvim
ln -snf ~/myConfigurations/.config/nvim ~/.config/nvim

# for linux from source
mkdir ~/Applications
cd ~/Applications
git clone https://github.com/neovim/neovim.git
cd neovim
git checkout v0.10.0
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install
ln -snf ~/myConfigurations/.config/nvim ~/.config/nvim

# for mac requirements
brew install ripgrep fd lazygit wget luarocks node
python3 -m pip install -U pip pynvim
npm install -g tree-sitter-cli
# not necessary but recommended
npm install -g n
ln -snf ~/myConfigurations/.config/nvim ~/.config/nvim
```

## zsh

``` bash
# install zsh, omz and change shell to it
sudo apt update && sudo apt install zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
sudo chsh -s $(which zsh)
# plugins
export ZSH=$HOME/.oh-my-zsh
curl -sS https://starship.rs/install.sh | sh
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-completions.git $ZSH/custom/plugins/zsh-completions
git clone https://github.com/zsh-users/zsh-autosuggestions.git $ZSH/custom/plugins/zsh-autosuggestions
git clone https://github.com/jeffreytse/zsh-vi-mode.git $ZSH/custom/plugins/zsh-vi-mode
# extra tools
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

## git

``` bash
ln -snf ~/myConfigurations/git/config ~/.gitconfig
```
## git ssh command

```bash
# add special ssh key to git config of project
git config core.sshCommand 'ssh -i ~/.ssh/id_ed25519_personal_github'
# use special ssh key in command
GIT_SSH_COMMAND='ssh -i ~/.ssh/id_ed25519_personal_github' git push
```

## unused

### tmux

``` bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -snf ~/myConfigurations/tmux.conf ~/.tmux.conf
```

### fonts

```bash
mkdir ~/.local/share/fonts
cd ~/.local/share/fonts
wget https://rubjo.github.io/victor-mono/VictorMonoAll.zip
uzip VictorMonoAll.zip
rm VicrorMonoAll.zip
wget https://github.com/microsoft/cascadia-code/releases/download/v2404.23/CascadiaCode-2404.23.zip
mkdir cascadia
unzip CascadiaCode-2404.23.zip -d cascadia
mv cascadia/ttf/*.ttf .
rm -rf CascadiaCode-2404.23.zip cascadia
fc-cache -fv
```

### ideavimrc

```bash
ln -snf ~/myConfigurations/ideavimrc ~/.ideavimrc
```

### emacs

```bash
sudo apt update && sudo apt install libmagickwand-dev libwebkit2gtk-4.0-dev
mkdir ~/Applications
cd ~/Applications
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs
git checkout emacs-29.3
sudo apt build-dep emacs
./autogen.sh
./configure --with-native-compilation=aot \
            --with-tree-sitter \
            --with-rsvg \
            --with-imagemagick \
            --with-x-toolkit=gtk3 \
            --with-cairo \
            --with-modules \
            --with-harfbuzz \
            --with-gnutls \
            --with-xml2 \
            --with-lcms2 \
            --with-xwidgets \
            --with-sound=alsa
make
make install
ln -snf ~/myConfigurations/emacs ~/.config/emacs
# to map caps lock to ctrl in debian
```

### alacritty

``` bash
ln -snf ~/myConfigurations/alacritty ~/.config/alacritty
```
