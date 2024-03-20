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
ln -snf ~/myConfigurations/nvim ~/.config/nvim
wget https://github.com/neovim/neovim/releases/download/v0.9.5/nvim.appimage
chmod u+x nvim.appimage
sudo mv nvim.appimage /usr/local/bin/nvim
grep -q 'alias vim="nvim"' ~/.zshrc || echo 'alias vim="nvim"' >> ~/.zshrc; grep -q 'alias vi="nvim"' ~/.zshrc || echo 'alias vi="nvim"' >> ~/.zshrc
```

# zsh
``` bash
ln -snf ~/myConfigurations/zsh/zshrc ~/.zshrc
```

# ideavimrc
```bash
ln -snf ~/myConfigurations/ideavimrc ~/.ideavimrc
```
