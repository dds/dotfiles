# Platform detection
case "$(uname -s)" in
    Linux*) machine=linux ;;
    Darwin*) machine=mac ;;
    CYGWIN*) machine=cygwin ;;
    MINGW*) machine=mingw ;;
    *) machine="UNKNOWN:$(uname -s)" ;;
esac

# Colors
[ -f ~/.dircolors ] && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"

# Platform-specific aliases
if [ "$machine" = "mac" ]; then
    alias ls='gls --color=auto'
else
    alias ls='ls --color=auto'
fi

# Listings
alias L='ls -Flb'
alias LL='ls -FLlb'
alias l='ls -Fb'
alias la='ls -Flab'
alias ll='ls -Flb'
alias lh='ls -Flh'

# Navigation
alias ot='popd'
alias to='pushd'

# Shortcuts
alias a='alias'
alias e="\$EDITOR"
alias h='history'
alias j='jobs -l'
alias k="kubectl"
alias r='cd / && screen -RD'
alias sudo='sudo -E'

# Tree
alias tree='tree -C'
alias treel='tree -phugD'
alias t='tree'
alias tl='treel'

# Prefer ripgrep
if command -v rg >/dev/null 2>&1; then
    alias grep="rg"
    alias g="rg"
fi

# SSH keychain (quiet unless error)
command -v keychain >/dev/null 2>&1 && \
    eval "$(keychain --eval --quiet ssh "$HOME/.ssh/id_ed25519")"
