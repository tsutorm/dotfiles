# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PS1="\[\033[0m\][\u@ \W]\[\033[36m\]\$(__git_ps1)\[\033[00m\]\$ "
#PS1="\[\033[1;32m\]\$(date +%Y/%m/%d_%H:%M:%S)\[\033[0m\] \[\033[33m\]\H:\w\n\[\033[0m\][\u@ \W]\[\033[36m\]\$(__git_ps1)\[\033[00m\]\$ "

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

# ssh-agent
echo -n "ssh-agent: "
source ~/.ssh-agent-info
ssh-add -l >&/dev/null
if [ $? == 2 ] ; then
    echo -n "ssh-agent: restart...."
    ssh-agent >~/.ssh-agent-info
    source ~/.ssh-agent-info
fi

if ssh-add -l >&/dev/null ; then
    echo "ssh-agent: Identity is already stored."
else
    ssh-add
fi


PATH=$PATH:$HOME/bin
TERM="xterm-256color"
export PATH
export TERM
alias emacs=/usr/local/bin/emacs
if [ -f $HOME/.bashrc ]; then
source $HOME/.bashrc
fi
