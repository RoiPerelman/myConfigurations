# should be in ~/.oh-my-zsh/themes/roip.zsh-theme

PROMPT="%{$fg_bold[blue]%}%n%{$reset_color%}" # username
PROMPT+="%{$fg_bold[cyan]%}@%{$reset_color%}" # '@' symbol
PROMPT+="%{$fg_bold[blue]%}%m%{$reset_color%}" # hostname
PROMPT+="%{$fg_bold[cyan]%}::%{$reset_color%}" # '::' separator
PROMPT+="%{$fg_bold[cyan]%}%~%{$reset_color%}" # directory
PROMPT+=" $(git_prompt_info)" # git info
# Newline and prompt symbol
PROMPT+="
%(?:%{$fg_bold[green]%}%1{➜%} :%{$fg_bold[red]%}%1{➜%} ) %{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[magenta]%} %{$fg_bold[magenta]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}%1{✗%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
