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

The bootstrap installer deploys these packages: floorp, fonts, **fuzzel**, git, kitty, latex, **mako**, nvim, pmux, rofi, scripts, shell, **sway**, tmux, vwm, **waybar**, xprofile-desktop, xresources.

### Bootstrap System

- `bootstrap/arch/pre-install.sh` — Phase 1: runs inside `artix-chroot`, creates user, generates SSH key, clones dotfiles
- `bootstrap/arch/install.sh` — Phase 2: post-reboot installer (interactive whiptail dialogs, supports `--dry-run`). Uses paru (AUR helper) and sigil (replaces loom for theme compilation)

### Theme System (Loom)

Loom is a custom theme manager that propagates a single theme (currently gruvbox) across applications. Generated theme files live in `~/.local/state/loom/generated/` and are sourced by kitty, tmux, rofi, and the vwm status bar. Neovim's theme is generated to `nvim/.config/nvim/lua/oracle/generated/theme.lua`.

### Custom Tools

- **vwm** — Custom tiling window manager (built from external repo during bootstrap; X11)
- **loom / loom-ui** — Theme manager binaries
- **pmux / pmux-run / pmux-cheat** — Project multiplexer for tmux sessions
- **kara-toe-client** — Foot-fork terminal vendored at `~/repos/kara/crates/kara-toe/` and installed via `kara` repo. Default `$TERMINAL` system-wide.

## Sway (Wayland) Session

The Wayland desktop is sway + waybar + fuzzel + mako, ported off the kara compositor. Launched via `start-sway` (in `sway/.local/bin/`, also see "Stow notes" below). The kara binary still works in parallel for fallback.

### Layout

- **Sway**: `sway/.config/sway/config` is the master config; per-machine overrides go in `~/.config/sway/config.d/*.conf` (not stowed — those are wallpaper/cursor persisted choices).
- **Waybar**: `waybar/.config/waybar/{config.jsonc,style.css}`. Glyphs are written as JSON `\u` escapes for portability across machines with different Nerd Font versions.
- **Fuzzel** (launcher → `mod+d`): `fuzzel/.config/fuzzel/fuzzel.ini`.
- **Mako** (notifications): `mako/.config/mako/config`.

### Sway helper scripts (in `sway/.local/bin/`)

These are NOT stowed via the package's `.local/bin` — sway's `.stow-local-ignore` blocks `.local` because the `scripts` package has pre-existing absolute-symlink artifacts that abort cross-package stow. They're symlinked manually one by one. Bootstrap should `ln -sf` each:

- **`start-sway`** — TTY launcher. Wraps in `dbus-run-session` if no user bus exists, exports Wayland/Qt/Mozilla/Java env hints, refuses to launch if `WAYLAND_DISPLAY` is set (prevents accidental nesting in another compositor). Auto-detects evdi (DisplayLink) and passes `--unsupported-gpu`. Logs to `~/.cache/sway/start-sway.log`.
- **`sway-output-profile`** — Detects connected outputs and applies docked vs. undocked profile. Daemonizes after the initial pass to handle hotplug. Consolidates orphan `<N>:<output>` workspaces from disconnected outputs into the surviving primary so the bar doesn't show duplicate workspace numbers.
- **`sway-ws`** — Per-output workspace switcher. Workspaces are named `<N>:<output>` (e.g. `1:DVI-I-1`, `1:eDP-1`) so each monitor has its own independent 1–9. `mod+1`–`mod+9` go through this script; falls through `swaymsg workspace`.
- **`sway-monocle`** — Kara-style monocle. Moves all but the focused window to a hidden per-workspace holding workspace named `__mono_<ws>__` (NOT scratchpad — scratchpad forces floating, which causes visible jumps). `cycle-next`/`cycle-prev` swaps focused with hidden. `is-active` returns 0 when active so keybinds can chain `sway-monocle cycle-next || sway-focus-cycle next`.
- **`sway-focus-cycle`** — Robust cycle through tiled windows on the current workspace. Replaces `focus next sibling` (too shallow in deep autotiling trees) and `focus next` (jumps unexpectedly).
- **`sway-zoom-master`** — Swaps focused window with the leftmost window in the current workspace. Mirrors kara's `zoom_master` / dwm's master promote. Pairs with autotiling-rs's fib layout where the leftmost window IS the master.
- **`sway-kill`** — `mod+q` wrapper. Aware of monocle: if killing the visible window leaves the workspace empty but hidden monocle windows exist, auto-restores one to view.
- **`sway-wallpaper`** / **`sway-cursor`** — Preview-cycle pickers driven by sway modes (`mod+Shift+w` / `mod+Shift+c`). Left/Right cycles with live preview, Enter commits, Esc reverts. State persists to `~/.config/sway/config.d/{wallpaper,cursor}.conf`.

### Output topology (Charlton's machine)

Three monitors when docked, one when not:
- `DVI-I-2` — left portrait (1920×1080, transform 270, position 0,0). On evdi (DisplayLink).
- `DVI-I-1` — center 1440p (2560×1440@144Hz, position 1080,0, primary). On evdi.
- `DP-2` — right portrait (1920×1080, transform 90, position 3640,0). Direct on amdgpu.
- `eDP-1` — laptop panel; only enabled when externals are absent.

### Fib (master/stack) layout

Sway has no native fibonacci — `autotiling-rs` (AUR: `autotiling-rs-git`) provides it by alternating splith/splitv based on the focused container's aspect ratio. `exec` line in the sway config silently no-ops if neither `autotiling` nor `autotiling-rs` is installed.

### Keybind quick-reference (kara → sway)

| kara                    | sway                         |
|-------------------------|------------------------------|
| `mod+Return`            | terminal (kara-toe-client)   |
| `mod+d`                 | fuzzel launcher              |
| `mod+e`                 | yazi in kara-toe-client      |
| `mod+f`                 | sway-monocle toggle          |
| `mod+Shift+f`           | output-fullscreen            |
| `mod+j` / `mod+k`       | cycle (monocle-aware)        |
| `mod+h` / `mod+l`       | focus output left/right      |
| `mod+Shift+h/l`         | move container to L/R output |
| `mod+Shift+Return`      | sway-zoom-master             |
| `mod+t`                 | floating toggle              |
| `mod+q`                 | sway-kill                    |
| `mod+1`–`mod+9`         | sway-ws switch (per-output)  |
| `mod+Shift+1`–`9`       | sway-ws move                 |
| `mod+apostrophe`        | scratchpad-main              |
| `mod+semicolon`         | scratchpad-music (spotatui)  |
| `mod+Shift+x`           | swaylock                     |
| `mod+Shift+w`           | wallpaper picker mode        |
| `mod+Shift+c`           | cursor picker mode           |
| `Print`                 | grim full-screen             |
| `mod+Shift+s`           | grim region → wl-copy        |

### Things kara had that sway doesn't replicate

- `sync_workspaces` toggle (`mod+s`) — sway has no concept; per-output independent is the only mode.
- Theme switcher (`mod+Shift+t`) — kara-beautify ran across many apps; needs a sway target in loom-rs to replicate.
- Keybind overlay (`mod+slash`) — sway has no built-in; could wire fuzzel-based later.

### Stow notes

The `sway` package has a `.stow-local-ignore` excluding `.local` because the `scripts` package contains absolute-symlink entries (`claude`, `python3.11`) that stow globally refuses, and would abort the sibling-tree analysis when `sway/.local/bin/` is included. Workaround: `ln -sf` the helper scripts into `~/.local/bin/` directly. The bootstrap installer should do this after `stow_dotfiles`.

### Key Conventions

- XDG Base Directory spec is followed throughout
- Default editor: Neovim (nightly), terminal: Kitty, browser: Floorp, shell: Zsh
- Neovim config is modular Lua with lazy.nvim plugin manager
- Makefile uses `>` as recipe prefix instead of TAB (`.RECIPEPREFIX := >`)
- Binaries built from source (vwm, loom, pmux) are gitignored; only wrapper scripts are tracked in `scripts/`
