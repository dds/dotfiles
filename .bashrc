test -f /etc/bashrc && source /etc/bashrc
test -f /usr/facebook/ops/rc/master.bashrc && source /usr/facebook/ops/rc/master.bashrc

### Sensible Bash - An attempt at saner Bash defaults
# Maintainer: mrzool <http://mrzool.cc>
# Repository: https://github.com/mrzool/bash-sensible
# Version: 0.2.2

# Unique Bash version check
if ((BASH_VERSINFO[0] < 4)); then
    echo "sensible.bash: Looks like you're running an older version of Bash."
    echo "sensible.bash: You need at least bash-4.0 or some options will not work correctly."
    echo "sensible.bash: Keep your software up-to-date!"
fi

## GENERAL OPTIONS ##

# Prevent file overwrite on stdout redirection
# Use `>|` to force redirection to an existing file
set -o noclobber

# Update window size after every command
shopt -s checkwinsize

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# Turn on recursive globbing (enables ** to recurse all directories)
shopt -s globstar 2>/dev/null

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

## SANE HISTORY DEFAULTS ##

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

_setup_keybindings() {
    ### KEYBINDINGS
    # Enable history expansion with space
    # E.g. typing !!<space> will replace the !! with your last command
    bind Space:magic-space

    # Perform file completion in a case insensitive fashion
    bind "set completion-ignore-case on"

    # Treat hyphens and underscores as equivalent
    bind "set completion-map-case on"

    # Display matches for ambiguous patterns at first tab press
    bind "set show-all-if-ambiguous on"

    # Immediately add a trailing slash when autocompleting symlinks to directories
    bind "set mark-symlinked-directories on"

    ## SMARTER TAB-COMPLETION (Readline bindings) ##
    # Enable incremental history search with up/down arrows (also Readline goodness)
    # Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
    bind '"\e[C": forward-char'
    bind '"\e[D": backward-char'
}

## BETTER DIRECTORY NAVIGATION ##

# Prepend cd to directory names automatically
shopt -s autocd 2>/dev/null
# Correct spelling errors during tab-completion
shopt -s dirspell 2>/dev/null
# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2>/dev/null

# This defines where cd looks for targets
# Add the directories you want to have fast access to, separated by colon
# Ex: CDPATH=".:~:~/projects" will look for targets in the current working directory, in home and in the ~/projects folder
CDPATH=".:~:~/src"

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars

## Functions

sum() {
    # Add up all arguments as numbers and print the result.
    awk '{ sum +=$1 } END {printf "%0.3f", sum}'
}

## Source modules

# Polyglot prompt (vendored from https://github.com/agkozak/polyglot)
[ -r ~/.bash/polyglot.sh ] && . ~/.bash/polyglot.sh

# Interactive setup
case $- in
    *i*)
        type _polyglot_main &>/dev/null && _polyglot_main
        _setup_keybindings
        ;;
    *) ;;
esac

# Clean up polyglot internals
unset -f _polyglot_is_ssh _polyglot_is_busybox _polyglot_is_dtksh \
    _polyglot_is_pdksh 2>/dev/null

# Aliases, platform detection, keychain
[ -r ~/.bash/aliases.bash ] && . ~/.bash/aliases.bash

