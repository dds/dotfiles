#!/bin/bash
case "$(uname -s)" in
Linux*) machine=linux ;;
Darwin*) machine=mac ;;
CYGWIN*) machine=cygwin ;;
MINGW*) machine=mingw ;;
*) machine="UNKNOWN:${unameOut}" ;;
esac

export EDITOR="emacsclient"
export GOPATH=~/code/go
export GONOSUMDB=github.com/NerdWallet
export GPG_KEY=0x22B8564124FA9655
export GTK_IM_MODULE="ibus"
export PASSWORD_STORE_DIR=~/Sync/pass
export PATH=~/.local/bin:~/.emacs.d/bin:~/.cargo/bin:$GOPATH/bin:$PATH
export PGP_KEY=$GPG_KEY
export QT_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"

if [ "$machine" = "mac" ]; then
	export GOROOT="$(brew --prefix go)/libexec"
fi

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

test -f "$HOME/.sdkman/bin/sdkman-init.sh" && source "$HOME/.sdkman/bin/sdkman-init.sh"

test -f "$HOME/.cargo/env" && source "$HOME/.cargo/env" && export PATH="$PATH:`dirname $(rustup which rustc)`"

gcloud_sdk="/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/"
test -f "$gcloud_sdk/path.bash.inc" && source "$gcloud_sdk/path.bash.inc"
test -f "$gcloud_sdk/completion.bash.inc" && source "$gcloud_sdk/completion.bash.inc"

if echo "$0" | grep -q bash >/dev/null; then
	if [ -r ~/.bashrc ]; then
		. ~/.bashrc
	fi
fi
