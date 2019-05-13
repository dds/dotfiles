#!/bin/bash
case "$(uname -s)" in
    Linux*)     machine=linux;;
    Darwin*)    machine=mac;;
    CYGWIN*)    machine=cygwin;;
    MINGW*)     machine=mingw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
export EDITOR="emacsclient"
export GTK_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE="ibus"
export GOPATH=~/code/go
if [ "$machine" = "mac" ]; then
    GOROOT="`brew --prefix go@1.11`/libexec"
else
    GOROOT=/usr/local/go
fi
export GOROOT
export PASSWORD_STORE_DIR=~/Sync/Private/PassStore
export PATH=~/src/mu/mu:~/.cargo/bin:$GOPATH/bin:$GOROOT/bin:~/.local/bin:/usr/local/opt/node@6/bin:$PATH
export GPG_KEY=0x22B8564124FA9655
export PGP_KEY=$GPG_KEY

_is_ssh() {
    [ -n "${SSH_CONNECTION-}${SSH_CLIENT-}${SSH_TTY-}" ]
}

# if ( ! _is_ssh && which gpgconf && which gpg-agent ) >/dev/null; then
#     declare SSH_AUTH_SOCK
#     SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
#     if [ $? -eq 0 ]; then
#         export SSH_AUTH_SOCK
#         gpgconf --launch gpg-agent
#     fi
# fi

if echo "$0" | grep -q bash >/dev/null; then
    if [ -r ~/.bashrc ]; then
        . ~/.bashrc
    fi
fi

export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && source "/Users/dsmith/.sdkman/bin/sdkman-init.sh"
