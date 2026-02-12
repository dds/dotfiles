source /etc/bashrc
source /usr/facebook/ops/rc/master.bashrc

# .bash_profile — sourced by bash login shells
# Sources .profile (env vars) then .bashrc (interactive settings)

[ -r ~/.profile ] && . ~/.profile
[ -r ~/.bashrc ] && . ~/.bashrc
