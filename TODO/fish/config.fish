###-------------------------------
###     environment variables
###-------------------------------
set -x NODE_REPL_HISTORY ~/.config/node/.node_repl_history
set -x JAVA_HOME /Library/Java/JavaVirtualMachines/jdk1.8.0_152.jdk/Contents/Home
set -x NVM_DIR ~/.config/nvm
set -x EDITOR vim
set -x -U GOPATH $HOME/Work/other/go
set -x -U GOBIN $GOPATH/bin
set -x PATH $PATH $GOBIN

###-----------------
###     aliases
###-----------------

# fish uses functions as aliases
alias aliases "functions"

alias vi vim

alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

alias ccat='pygmentize -g'

alias cheatsheet "code ~/myConfigurations/Cheatsheets"

alias mytodo "mdv ~/myConfigurations/Cheatsheets/mytodo.md"
alias mytodosheet "code ~/myConfigurations/Cheatsheets/mytodo.md"

alias mycheat "mdv ~/myConfigurations/Cheatsheets/mycheat.md"
alias mycheatsheet "code ~/myConfigurations/Cheatsheets/mycheat.md"

alias myvim "mdv ~/myConfigurations/Cheatsheets/myvim.md"
alias myvimsheet "code ~/myConfigurations/Cheatsheets/myvim.md"

alias mygit "mdv ~/myConfigurations/Cheatsheets/mygit.md"
alias mygitsheet "code ~/myConfigurations/Cheatsheets/mygit.md"

alias myDY "mdv ~/myConfigurations/Cheatsheets/myDY.md"
alias myDYsheet "code ~/myConfigurations/Cheatsheets/myDY.md"

alias myvagrant "mdv ~/myConfigurations/Cheatsheets/myvagrant.md"
alias myvagrantsheet "code ~/myConfigurations/Cheatsheets/myvagrant.md"

alias mynginx "mdv ~/myConfigurations/Cheatsheets/mynginx.md"
alias mynginxsheet "code ~/myConfigurations/Cheatsheets/mynginx.md"

alias mychef "mdv ~/myConfigurations/Cheatsheets/mychef.md"
alias mychefsheet "code ~/myConfigurations/Cheatsheets/mychef.md"

alias myredis "mdv ~/myConfigurations/Cheatsheets/myredis.md"
alias myredissheet "code ~/myConfigurations/Cheatsheets/myredis.md"

alias mypass "mdv ~/myConfigurations/Cheatsheets/mypass.md"
alias mypasssheet "code ~/myConfigurations/Cheatsheets/mypass.md"

alias mysqldb "mdv ~/myConfigurations/Cheatsheets/mysqldb.md"
alias mysqldbsheet "code ~/myConfigurations/Cheatsheets/mysqldb.md"

alias mydocker "mdv ~/myConfigurations/Cheatsheets/mydocker.md"
alias mydockersheet "code ~/myConfigurations/Cheatsheets/mydocker.md"

alias mypython "mdv ~/myConfigurations/Cheatsheets/mypython.md"
alias mypythonsheet "code ~/myConfigurations/Cheatsheets/mypython.md"

alias myintellij "mdv ~/myConfigurations/Cheatsheets/myintellij.md"
alias myintellijsheet "code ~/myConfigurations/Cheatsheets/myintellij.md"

alias myclojure "mdv ~/myConfigurations/Cheatsheets/myclojure.md"
alias myclojuresheet "code ~/myConfigurations/Cheatsheets/myclojure.md"

alias myemacs "mdv ~/myConfigurations/Cheatsheets/myemacs.md"
alias myemacssheet "code ~/myConfigurations/Cheatsheets/myemacs.md"

###-----------------
###     Functions
###-----------------

function knl
        knife node list | grep $argv
end

###-----------------
###     plugins
###-----------------
# -> autojump
[ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish
