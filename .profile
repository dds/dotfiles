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


_is_ssh() {
    [ -n "${SSH_CONNECTION-}${SSH_CLIENT-}${SSH_TTY-}" ]
}

if ( ! _is_ssh && which gpgconf && which gpg-agent ) >/dev/null; then
   declare SSH_AUTH_SOCK
   SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
   if [[ $? -eq 0 ]]; then
       export SSH_AUTH_SOCK
       gpgconf --launch gpg-agent
   fi
fi

if echo "$0" | grep -q bash >/dev/null; then
    if [[ -r ~/.bashrc ]]; then
        . ~/.bashrc
    fi
fi
