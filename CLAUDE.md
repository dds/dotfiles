# Dotfiles

Personal dotfiles managed with Rake. Symlinks are created from dotfiles repo into `$HOME`.

## Structure

- `Rakefile` — install task creates directories and symlinks into a target prefix (default `$HOME`)
- `.config/kitty/` — Kitty terminal emulator config
- `.aerospace.toml` — AeroSpace tiling window manager config (macOS)
- `.bash_profile` / `.bashrc` / `.profile` — shell config
- `.screenrc` — GNU Screen config (version-specific color configs: `.screenrc.colors.v5` / `.screenrc.colors.legacy`)
- `Brewfile` — Homebrew dependencies
- `bin/` — personal scripts, symlinked to `~/.local/bin`
- `bootstrap/` — provisioning scripts

## Usage

```sh
# Install symlinks into $HOME
rake install[$HOME]

# Install Homebrew packages
brew bundle
```

## Terminal & WM

- **Terminal**: Kitty
- **Window manager**: AeroSpace (alt-based sway-inspired keybindings)
- **Multiplexer**: GNU Screen

## Pending

- Push to origin (2 commits ahead of `origin/master`): run `git push origin master`
</content>
