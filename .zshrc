if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Set the directory we want to store zinit and plugins
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download Zinit, if it's not there yet
if [ ! -d "$ZINIT_HOME" ]; then
   mkdir -p "$(dirname $ZINIT_HOME)"
   git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Source/Load zinit
source "${ZINIT_HOME}/zinit.zsh"

# https://gist.github.com/ketsuban/651e24c2d59506922d928c65c163d79c
# add easy keys for keybind. use like this "$key[Up]"
key=(
    Up         "${terminfo[kcuu1]}"
    Down       "${terminfo[kcud1]}"
    Left       "${terminfo[kcub1]}"
    Right      "${terminfo[kcuf1]}"
    CtrlRight  "${terminfo[kRIT5]}"
    CtrlLeft   "${terminfo[kLFT5]}"
)

# Load starship theme
# line 1: `starship` binary as command, from github release
# line 2: starship setup at clone(create init.zsh, completion)
# line 3: pull behavior same as clone, source init.zsh
zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh"
zinit light starship/starship

# Add in zsh plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions

# add vi mode to zsh
zinit ice depth=1
zinit light jeffreytse/zsh-vi-mode

# Add in snippets
zinit snippet OMZP::git
zinit snippet OMZP::aws
zinit snippet OMZP::command-not-found

# Load the necessary key bindings

# completion using arrow keys (based on history)
bindkey "$key[Up]" history-search-backward
bindkey "$key[Down]" history-search-forward
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward

# Keybindings fix for Ctrl+arrow keys
bindkey "${key[CtrlRight]}" forward-word        # Ctrl+Right Arrow
bindkey "${key[CtrlLeft]}"  backward-word       # Ctrl+Left Arrow

# History
HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Load completions
autoload -Uz compinit && compinit

zinit cdreplay -q

# Set LS_COLORS if not already defined
if [[ -z "$LS_COLORS" ]]; then
  if command -v dircolors >/dev/null 2>&1; then
    # dracula theme installs https://draculatheme.com/gnome-terminal
    if [ -f ~/.dir_colors/dircolors ]; then
      eval `dircolors ~/.dir_colors/dircolors`
    else
      eval "$(dircolors -b)"
    fi
  else
    # Default LS_COLORS settings
    export LS_COLORS="di=1;34:ln=36:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43"
  fi
fi

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}' 'm:{[:lower:][:upper:]-_}={[:upper:][:lower:]_-}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' special-dirs true

# Aliases
alias ls='ls --color'
alias ll='ls -lh'
alias vi='nvim'
alias vim='nvim'
alias c='clear'

# env variables
export PATH=~/.local/bin:$PATH
export EDITOR="nvim"
export VISUAL="nvim"
export MANPAGER='nvim +Man!'

if [[ "$(uname)" == "Linux" ]]; then
  # set caps-lock to ctrl
  setxkbmap -option ctrl:nocaps
fi

# fzf
if [ ! -f ~/.fzf.zsh ]; then
   git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
   ~/.fzf/install
fi
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# zoxide
if [ ! -f ~/.local/bin/zoxide ]; then
    curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | sh
fi
eval "$(zoxide init --cmd j zsh)"

# tinyrc
if [ -f ~/.tinyrc ]; then
    source ~/.tinyrc
fi

# virtualenvwrapper
if command -v virtualenvwrapper.sh &> /dev/null
then
    source virtualenvwrapper.sh
fi

# pyenv
if command -v pyenv > /dev/null; then
  export PYENV_ROOT="$HOME/.pyenv"
  [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
fi


# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
