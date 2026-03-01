# Dotfiles

Personal dotfiles repo managed with Rake. Configs for bash, vim, git, screen, ghostty, GPG, and systemd units.

## Install

```sh
rake install[$HOME]
```

This symlinks dotfiles from this repo into the target prefix directory.

## Structure

- **Rakefile** - Installation logic. Symlinks files listed in `symlinks` to the target prefix. Linux-only symlinks and systemd units are handled separately. Screen color config is version-detected (v5 vs legacy).
- **.bashrc** - Main shell config (bash-sensible defaults, history, keybindings, polyglot prompt).
- **.bash/** - Sourced modules: `aliases.bash`, `polyglot.sh` (vendored prompt).
- **.vimrc** - Vim configuration.
- **.config/ghostty/config** - Ghostty terminal config.
- **.gitconfig** - Git configuration.
- **bin/** - Scripts symlinked into `~/.local/bin`.
- **bootstrap** - Initial setup script.

## Conventions

- Dotfiles live at the repo root matching their home directory paths (e.g., `.bashrc`, `.vimrc`).
- Platform-specific logic uses `is_linux`/`is_mac` detection in the Rakefile.
- Screen colors have two variants: `.screenrc.colors.v5` (Screen 5+) and `.screenrc.colors.legacy`.
