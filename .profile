#!/bin/bash
export EDITOR="emacsclient"
export GTK_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE="ibus"
export GOPATH=~/go
export GOHOME=/usr/local/go
export PASSWORD_STORE_DIR=~/Sync/Private/PassStore
export PATH=~/util:~/src/mu/mu:~/.cargo/bin:$GOPATH/bin:$GOHOME/bin:~/.local/bin:$PATH
export GPG_KEY=0x22B8564124FA9655
export PGP_KEY=$GPG_KEY

_is_ssh() {
    [ -n "${SSH_CONNECTION-}${SSH_CLIENT-}${SSH_TTY-}" ]
}

if ( ! _is_ssh && which gpgconf && which gpg-agent ) >/dev/null; then
   declare SSH_AUTH_SOCK
   SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
   if [ $? -eq 0 ]; then
       export SSH_AUTH_SOCK
       gpgconf --launch gpg-agent
   fi
fi


if echo "$0" | grep -q bash >/dev/null; then
    if [ -r ~/.bashrc ]; then
        . ~/.bashrc
    fi
fi
