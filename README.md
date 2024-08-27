# PREREQUISITES

## tmux

``` bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -snf ~/myConfigurations/tmux/tmux.conf ~/.tmux.conf
```

## nvim

``` bash
# prereq linux for nvim
sudo apt update && sudo apt install cmake gettext
sudo apt update && sudo apt install ripgrep fd-find

mkdir ~/Applications
cd ~/Applications
git clone https://github.com/neovim/neovim.git
cd neovim
git checkout v0.10.0
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install
ln -snf ~/myConfigurations/nvim ~/.config/nvim

# for mac requirements
brew install ripgrep fd lazygit wget luarocks node
python3 -m pip install -U pip pynvim
npm install -g tree-sitter-cli
# not necessary but recommended
npm install -g n
brew install tmux
```

## zsh

``` bash
mv ~/.zshrc ~/.zshrc_bk
ln -snf ~/myConfigurations/.zshrc ~/.zshrc
mkdir -p ~/.config && ln -snf ~/myConfigurations/.config/starship.toml ~/.config/starship.toml

```

## git

``` bash
ln -snf ~/myConfigurations/git/config ~/.gitconfig
```

## fonts

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

## utilities

```bash
# to remove caps lock
python3 -c 'from ctypes import *; \
X11 = cdll.LoadLibrary("libX11.so.6"); \
display = X11.XOpenDisplay(None); \
X11.XkbLockModifiers(display, c_uint(0x0100), c_uint(2), c_uint(0)); \
X11.XCloseDisplay(display)'
```

## unused

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
