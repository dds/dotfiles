# Minimal zsh config — just enough to be comfortable if landed in zsh
# (e.g. macOS Catalina+ where zsh is the default)

## History (match bash settings)
HISTFILE=~/.zsh_history
HISTSIZE=500000
SAVEHIST=100000
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt SHARE_HISTORY
setopt APPEND_HISTORY

## Completion
autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'  # case-insensitive
zstyle ':completion:*' menu select
setopt COMPLETE_IN_WORD
setopt AUTO_MENU

## Directory navigation (match bash shopt settings)
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_SILENT
setopt CDABLE_VARS
CDPATH=".:~:~/src"

## Misc
setopt NO_CLOBBER          # match bash set -o noclobber
setopt INTERACTIVE_COMMENTS
setopt EXTENDED_GLOB

## Key bindings — emacs mode with history search on up/down
bindkey -e
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning-search-forward

## Colors
[ -f ~/.dircolors ] && eval "$(dircolors -b ~/.dircolors)" 2>/dev/null

## Prompt — simple git-aware prompt, no dependencies
autoload -Uz vcs_info
precmd() { vcs_info }
setopt PROMPT_SUBST
zstyle ':vcs_info:git:*' formats ' (%b)'
zstyle ':vcs_info:*' enable git

if [[ -n "$SSH_CONNECTION" ]]; then
    PROMPT='%F{green}%n@%m%f %F{blue}%3~%f%F{yellow}${vcs_info_msg_0_}%f %# '
else
    PROMPT='%F{green}%n%f %F{blue}%3~%f%F{yellow}${vcs_info_msg_0_}%f %# '
fi

## Platform detection
case "$(uname -s)" in
    Darwin*) alias ls='gls --color=auto' 2>/dev/null || alias ls='ls -G' ;;
    *) alias ls='ls --color=auto' ;;
esac

## Aliases (same as bash)
alias L='ls -Flb'
alias LL='ls -FLlb'
alias l='ls -Fb'
alias la='ls -Flab'
alias ll='ls -Flb'
alias lh='ls -Flh'
alias ot='popd'
alias to='pushd'
alias a='alias'
alias e="\$EDITOR"
alias h='history'
alias j='jobs -l'
alias k="kubectl"
alias r='cd / && screen -RD'
alias sudo='sudo -E'
alias tree='tree -C'
alias treel='tree -phugD'
alias t='tree'
alias tl='treel'

(( $+commands[rg] )) && alias grep="rg" && alias g="rg"

## Keychain
(( $+commands[keychain] )) && \
    eval "$(keychain --eval --quiet --agents ssh "$HOME/.ssh/id_ed25519")"
