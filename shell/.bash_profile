# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
export SSH_ASKPASS=/usr/bin/ksshaskpass

# disable interruprt and susp control sequences
stty -ixon

PATH=$PATH:$HOME/.local/bin:$HOME/bin
export PATH

export CDPATH=$HOME/work
