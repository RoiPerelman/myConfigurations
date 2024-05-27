# PREREQUISITES

## alacritty
``` bash
ln -snf ~/myConfigurations/alacritty ~/.config/alacritty
```

## tmux
``` bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
ln -snf ~/myConfigurations/tmux/tmux.conf ~/.tmux.conf
```

## git
``` bash
ln -snf ~/myConfigurations/git/config ~/.gitconfig
```

## nvim
``` bash
mkdir ~/Applications
cd ~/Applications
git clone https://github.com/neovim/neovim.git
cd neovim
git checkout v0.10.0
make CMAKE_BUILD_TYPE=RelWithDebInfo
sudo make install
ln -snf ~/myConfigurations/nvim ~/.config/nvim
```

# zsh
``` bash
mv ~/.zshrc ~/.zshrc_bk
ln -snf ~/myConfigurations/.zshrc ~/.zshrc
mkdir -p ~/.config && ln -snf ~/myConfigurations/.config/starship.toml ~/.config/starship.toml
```

# ideavimrc
```bash
ln -snf ~/myConfigurations/ideavimrc ~/.ideavimrc
```

# emacs
```bash
mkdir ~/Applications && cd ~/Applications
git clone https://git.savannah.gnu.org/git/emacs.git
cd emacs
git checkout emacs-29.3
sudo apt build-dep emacs
./autogen.sh
./configure --with-native-compilation=aot \                                                                                                                              20:31:22
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
ln -snf ~/myConfigurations/emacs ~/.config/emacs
# to map caps lock to ctrl in debian
```

# fonts
```
mkdir ~/.local/share/fonts
cd ~/.local/share/fonts
wget https://rubjo.github.io/victor-mono/VictorMonoAll.zip
uzip VictorMonoAll.zip
rm VicrorMonoAll.zip
fc-cache -fv
```
