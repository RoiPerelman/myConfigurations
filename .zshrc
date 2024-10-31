export ZSH="$HOME/.oh-my-zsh"

# load starship prompt
eval "$(starship init zsh)"

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

# load plugins
plugins=(
  zsh-syntax-highlighting
  zsh-completions
  zsh-autosuggestions
  zsh-vi-mode
  git
  aws
  command-not-found
)

# source oh-my-zsh
source $ZSH/oh-my-zsh.sh

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

# brew
if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# zoxide
eval "$(zoxide init --cmd j zsh)"

# virtualenvwrapper
if command -v virtualenvwrapper.sh &> /dev/null
then
    source virtualenvwrapper.sh
fi

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv > /dev/null; then
  eval "$(pyenv init -)"

  # virtualenv
  export WORKON_HOME=$HOME/.virtualenvs
  export VIRTUALENVWRAPPER_PYTHON=$(pyenv which python)
  export VIRTUALENVWRAPPER_SCRIPT=/home/roip/.pyenv/versions/3.11.7/bin/virtualenvwrapper.sh
  [ -f $VIRTUALENVWRAPPER_SCRIPT ] && source $VIRTUALENVWRAPPER_SCRIPT
fi

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# tinyrc
if [ -f ~/.tinyrc ]; then
    source ~/.tinyrc
fi

# inspektoconfig
if [ -f ~/.inspektoconfig ]; then
    source ~/.inspektoconfig
fi
