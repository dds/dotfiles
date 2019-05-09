#!/bin/bash
export EDITOR="emacsclient"
export GTK_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE="ibus"
export GOPATH=~/go
if [ "Darwin" == "$(uname)" ]; then
    export GOROOT="`brew --prefix go@1.11`/libexec"
else
    export GOROOT=/usr/local/go
fi
export PASSWORD_STORE_DIR=~/Sync/Private/PassStore
export PATH=~/src/mu/mu:~/.cargo/bin:$GOPATH/bin:$GOROOT/bin:~/.local/bin:/usr/local/opt/node@6/bin:$PATH
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
