SHELL := /usr/bin/env bash
DOTFILES_DIR := $(HOME)/.dotfiles
INSTALL := $(DOTFILES_DIR)/install.sh

# Allow recipes to start with ">" instead of a TAB.
.RECIPEPREFIX := >

.PHONY: install dry-run submodules-status submodules-update submodules-sync \
        suckless-build suckless-install tampermonkey-check tampermonkey-capture

install:
>   $(INSTALL)

dry-run:
>   $(INSTALL) --dry-run

submodules-status:
>   cd "$(DOTFILES_DIR)" && git submodule status --recursive

submodules-update:
>   cd "$(DOTFILES_DIR)" && git submodule update --init --recursive
>   cd "$(DOTFILES_DIR)" && git submodule update --remote --merge --recursive
>   cd "$(DOTFILES_DIR)" && git submodule status --recursive

submodules-sync: submodules-update

suckless-build:
>   cd "$(HOME)/code/dwm" && make clean && make
>   cd "$(HOME)/code/dmenu" && make clean && make
>   cd "$(HOME)/code/slstatus" && make clean && make

suckless-install:
>   cd "$(HOME)/code/dwm" && sudo make install
>   cd "$(HOME)/code/dmenu" && sudo make install
>   cd "$(HOME)/code/slstatus" && sudo make install

tampermonkey-check:
>   test -f "$(DOTFILES_DIR)/assets/tampermonkey/tampermonkey-backup.zip" && \
>       echo "OK: assets/tampermonkey/tampermonkey-backup.zip exists" || \
>       (echo "MISSING: assets/tampermonkey/tampermonkey-backup.zip" && exit 1)

tampermonkey-capture:
>   mkdir -p "$(DOTFILES_DIR)/assets/tampermonkey"
>   newest="$$(ls -t "$(HOME)"/Downloads/*tampermonkey*.zip 2>/dev/null | head -n 1)"; \
>   if [[ -z "$$newest" ]]; then \
>       echo "No Tampermonkey zip found in $(HOME)/Downloads. Export it from Tampermonkey -> Utilities -> Export first."; \
>       exit 1; \
>   fi; \
>   cp -f "$$newest" "$(DOTFILES_DIR)/assets/tampermonkey/tampermonkey-backup.zip"; \
>   echo "Captured: $$newest -> assets/tampermonkey/tampermonkey-backup.zip"
