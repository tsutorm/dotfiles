# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PS1="\[\033[1;32m\]\$(date +%Y/%m/%d_%H:%M:%S)\[\033[0m\] \[\033[33m\]\H:\w\n\[\033[0m\][\u@ \W]\[\033[36m\]\$(__git_ps1)\[\033[00m\]\$ "

#
# git-completion.bash / git-prompt.sh
#
if [ -f ~/.bash.d/git-completion.bash ]; then
    source ~/.bash.d/git-completion.bash
fi
if [ -f ~/.bash.d/git-prompt.sh ]; then
    source ~/.bash.d/git-prompt.sh
fi
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUPSTREAM=auto

PATH=$PATH:$HOME/bin
TERM="xterm-256color"
export PATH
export TERM
alias emacs=/usr/local/bin/emacs-24.3
PATH=/home/onishi.tsutomu/.cask/bin:/usr/lib/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/home/onishi.tsutomu/bin:/home/onishi.tsutomu/bin
