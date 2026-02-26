SHELL := /bin/bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

DOTFILES_DIR := $(abspath .)

.PHONY: help
help:
    @echo "Targets:"
    @echo "  make install        Run installer"
    @echo "  make dry-run        Run installer in dry-run mode"
    @echo "  make submodules     Init/update submodules"
    @echo "  make submodules-up  Pull latest upstream for submodules"
    @echo "  make suckless-link  Symlink suckless repos into ~/code"
    @echo "  make suckless-build Build dwm/dmenu/slstatus"
    @echo "  make suckless-install Install dwm/dmenu/slstatus (sudo)"
    @echo "  make suckless-all   Link + build + install"

install:
    "$(DOTFILES_DIR)/install.sh"

dry-run:
    "$(DOTFILES_DIR)/install.sh" --dry-run

submodules:
    cd "$(DOTFILES_DIR)"
    git submodule update --init --recursive

submodules-up:
    cd "$(DOTFILES_DIR)"
    git submodule update --remote --merge
    git status

suckless-link:
    mkdir -p "$(HOME)/code"
    rm -rf "$(HOME)/code/dwm" "$(HOME)/code/dmenu" "$(HOME)/code/slstatus"
    ln -s "$(DOTFILES_DIR)/suckless/dwm" "$(HOME)/code/dwm"
    ln -s "$(DOTFILES_DIR)/suckless/dmenu" "$(HOME)/code/dmenu"
    ln -s "$(DOTFILES_DIR)/suckless/slstatus" "$(HOME)/code/slstatus"

suckless-build:
    cd "$(HOME)/code/dwm" && make clean && make
    cd "$(HOME)/code/dmenu" && make clean && make
    cd "$(HOME)/code/slstatus" && make clean && make

suckless-install:
    sudo make -C "$(HOME)/code/dwm" install
    sudo make -C "$(HOME)/code/dmenu" install
    sudo make -C "$(HOME)/code/slstatus" install

suckless-all: suckless-link suckless-build suckless-install