HERE := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
PREFIX ?= $(HOME)
UNAME := $(shell uname -s)

DIRS := \
	.ssh \
	.gnupg \
	.config/kitty \
	.config/Code/User \
	.local/bin

SYMLINKS := \
	.bash \
	.bashrc \
	.bash_profile \
	.profile \
	.doom.d \
	.screenrc \
	.spacemacs.d \
	.aerospace.toml \
	.gitconfig \
	.gitconfig-fbinfra \
	.dircolors \
	.vimrc \
	.zshrc \
	.gnupg/gpg-agent.conf \
	.gnupg/gpg.conf \
	.config/kitty/kitty.conf \
	.config/Code/User/settings.json

LINUX_SYMLINKS := \
	.Xresources \
	.local/share/applications/org-protocol.desktop

SYSTEMD_UNITS := \
	keybase.service \
	kbfs.service \
	syncthing.service

.PHONY: install
install: dirs symlinks bin screenrc
ifeq ($(UNAME),Linux)
install: linux-symlinks
endif

.PHONY: full
full: install
ifeq ($(UNAME),Linux)
full: linux-desktop
endif

.PHONY: dirs
dirs:
	@mkdir -p $(PREFIX)
	@$(foreach d,$(DIRS),mkdir -p $(PREFIX)/$(d);)

.PHONY: symlinks
symlinks: dirs
	@$(foreach f,$(SYMLINKS),\
		if [ -e $(HERE)$(f) ]; then \
			rm -f $(PREFIX)/$(f); \
			ln -sf $(HERE)$(f) $(PREFIX)/$(f); \
		fi;)

.PHONY: bin
bin: dirs
	@for f in $(HERE)bin/*; do \
		[ -e "$$f" ] && ln -sf "$$f" $(PREFIX)/.local/bin/; \
	done; true

.PHONY: screenrc
screenrc:
	@SCREEN_V5=false; \
	ver=$$(screen --version 2>/dev/null); \
	case "$$ver" in \
		*"Screen version 5"*|*"Screen version 6"*|*"Screen version 7"*|*"Screen version 8"*|*"Screen version 9"*) \
			SCREEN_V5=true;; \
	esac; \
	if $$SCREEN_V5; then \
		src=".screenrc.colors.v5"; \
	else \
		src=".screenrc.colors.legacy"; \
	fi; \
	rm -f $(PREFIX)/.screenrc.colors; \
	ln -sf $(HERE)$$src $(PREFIX)/.screenrc.colors

.PHONY: linux-symlinks
linux-symlinks:
	@mkdir -p $(PREFIX)/.local/share/applications
	@$(foreach f,$(LINUX_SYMLINKS),\
		if [ -e $(HERE)$(f) ]; then \
			rm -f $(PREFIX)/$(f); \
			ln -sf $(HERE)$(f) $(PREFIX)/$(f); \
		fi;)

.PHONY: linux-desktop
linux-desktop:
	@mkdir -p $(PREFIX)/.config/systemd/user
	@for f in $(HERE).config/systemd/user/*; do \
		[ -e "$$f" ] && ln -sf "$$f" $(PREFIX)/.config/systemd/user/; \
	done; true
	@mkdir -p $(PREFIX)/.config/autostart
	@for f in $(HERE).config/autostart/*; do \
		[ -e "$$f" ] && ln -sf "$$f" $(PREFIX)/.config/autostart/; \
	done; true
	@$(foreach u,$(SYSTEMD_UNITS),systemctl --user enable $(u) || true;)
	@update-desktop-database $(PREFIX)/.local/share/applications || true
