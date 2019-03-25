#!/bin/sh
export EDITOR="emacsclient -n"
export ALTERNATE_EDITOR=vi
export EMACS_SERVER_FILE=$HOME/.emacs.d/server/server
export GTK_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE="ibus"
export GOPATH=~/go
export GOHOME=/usr/local/go
export PASSWORD_STORE_DIR=~/Sync/Private/PassStore
export PATH=~/util:~/src/mu/mu:~/.cargo/bin:$GOPATH/bin:$GOHOME/bin:~/.local/bin:$PATH

if echo "$SHELL" | grep -q bash; then
   declare SSH_AUTH_SOCK
   SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
   if [[ $? -eq 0 ]]; then
       export SSH_AUTH_SOCK
       gpgconf --launch gpg-agent
   fi
   if [[ -r ~/.bashrc ]]; then
       . ~/.bashrc
   fi
fi
