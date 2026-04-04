# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles managed with **GNU Stow**. Each top-level directory is a stow package that mirrors the home directory structure (`.config/`, `.local/`, etc.). Deploying a package symlinks its contents into `$HOME`.

## Common Commands

```bash
# Deploy a single package
stow -v -R -t "$HOME" <package-name>

# Full install (Artix Linux)
make install

# Dry-run install
make dry-run

# Capture Tampermonkey backup from Downloads
make tampermonkey-capture
```

## Architecture

### Stow Packages

Each top-level directory (e.g., `nvim/`, `shell/`, `tmux/`) is a standalone stow package. The directory structure inside each package mirrors `$HOME`. For example, `nvim/.config/nvim/init.lua` symlinks to `~/.config/nvim/init.lua`.

The bootstrap installer deploys these packages: floorp, fonts, git, kitty, latex, nvim, pmux, rofi, scripts, shell, tmux, vwm, xprofile-desktop, xresources.

### Bootstrap System

- `bootstrap/arch/pre-install.sh` — Phase 1: runs inside `artix-chroot`, creates user, generates SSH key, clones dotfiles
- `bootstrap/arch/install.sh` — Phase 2: post-reboot installer (interactive whiptail dialogs, supports `--dry-run`). Uses paru (AUR helper) and sigil (replaces loom for theme compilation)

### Theme System (Loom)

Loom is a custom theme manager that propagates a single theme (currently gruvbox) across applications. Generated theme files live in `~/.local/state/loom/generated/` and are sourced by kitty, tmux, rofi, and the vwm status bar. Neovim's theme is generated to `nvim/.config/nvim/lua/oracle/generated/theme.lua`.

### Custom Tools

- **vwm** — Custom tiling window manager (built from external repo during bootstrap)
- **loom / loom-ui** — Theme manager binaries
- **pmux / pmux-run / pmux-cheat** — Project multiplexer for tmux sessions

### Key Conventions

- XDG Base Directory spec is followed throughout
- Default editor: Neovim (nightly), terminal: Kitty, browser: Floorp, shell: Zsh
- Neovim config is modular Lua with lazy.nvim plugin manager
- Makefile uses `>` as recipe prefix instead of TAB (`.RECIPEPREFIX := >`)
- Binaries built from source (vwm, loom, pmux) are gitignored; only wrapper scripts are tracked in `scripts/`
