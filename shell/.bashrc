# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
source ~/.git-prompt.sh
export GIT_PS1_showupstream=auto
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWCOLORHINTS=1
export PROMPT_COMMAND='__git_ps1 "[\u@\h \W]" "\\\\$ "'

# Creates git repostitory with some reasonable default settings in current folder
function git_here() {
    git init
    git config color.ui auto
    echo "log tmp db/*.sqlite3 bin" | tr " " "\n" > .gitignore
    git add .gitignore
    git commit -m "initial project setup"
}
alias gh=git_here

# I want awesome to meke emacsclient frame always floating. Having proper title
# helps to match awesome rules
alias ec="emacsclient -n -c -F '((title . \"EmacsClient\"))'"
alias xc="xclip -selection clipboard"

# lein completion
source ~/.lein-completion.bash
