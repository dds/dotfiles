export EDITOR=vim
export VISUAL=vim

test -f "$HOME/.sdkman/bin/sdkman-init.sh" && source "$HOME/.sdkman/bin/sdkman-init.sh"

test -f "$HOME/.cargo/env" && source "$HOME/.cargo/env"
command -v rustup >/dev/null && export PATH="$PATH:$(dirname "$(rustup which rustc)")"

gcloud_sdk="/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/"
test -f "$gcloud_sdk/path.bash.inc" && source "$gcloud_sdk/path.bash.inc"
test -f "$gcloud_sdk/completion.bash.inc" && source "$gcloud_sdk/completion.bash.inc"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
