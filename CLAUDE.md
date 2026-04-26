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

The bootstrap installer deploys these packages: floorp, fonts, **fuzzel**, git, latex, **mako**, nvim, pmux, rofi, scripts, shell, **sway**, tmux, **waybar**, xdg, xinit-desktop, xresources.

### Bootstrap System

- `bootstrap/arch/pre-install.sh` — Phase 1: runs inside `artix-chroot`, creates user, generates SSH key, clones dotfiles
- `bootstrap/arch/install.sh` — Phase 2: post-reboot installer (interactive whiptail dialogs, supports `--dry-run`). Uses paru (AUR helper) and sigil (replaces loom for theme compilation)

### Theme System (Loom)

Loom is a custom theme manager that propagates a single theme (currently gruvbox) across applications. Generated theme files live in `~/.local/state/loom/generated/` and are sourced by tmux and rofi. Neovim's theme is generated to `nvim/.config/nvim/lua/oracle/generated/theme.lua`.

### Custom Tools

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
- **`sway-monocle`** — Kara-style monocle via native sway `fullscreen`. The focused window is fullscreened; other windows stay in their tile positions but are hidden. `cycle-next`/`cycle-prev` atomically swap fullscreen to a different leaf in a single swaymsg IPC call (disable fs, focus target, enable fs), so cycling is near-instant. Previous implementation moved windows between a `__mono_<ws>__` hidden workspace — too slow (4 IPC calls per cycle) and polluted waybar with a `-1` workspace button. `is-active` returns 0 when active so keybinds can chain `sway-monocle cycle-next || sway-focus-cycle next`.
- **`sway-focus-cycle`** — Robust cycle through tiled windows on the current workspace. Replaces `focus next sibling` (too shallow in deep autotiling trees) and `focus next` (jumps unexpectedly).
- **`sway-zoom-master`** — Swaps focused window with the leftmost window in the current workspace. Mirrors kara's `zoom_master` / dwm's master promote. Pairs with autotiling-rs's fib layout where the leftmost window IS the master.
- **`sway-float-toggle`** — `mod+t` wrapper. Toggling a tiled window to floating sizes it to ~70×75% of the output and centers it; toggling a floating window back to tiled is plain `floating disable`. Replaces sway's bare `floating toggle` which inherits the tile-time geometry (often a skinny strip).
- **`sway-wallpaper`** / **`sway-cursor`** — Preview-cycle pickers driven by sway modes (`mod+Shift+w` / `mod+Shift+c`). Left/Right cycles with live preview, Enter commits, Esc reverts. State persists to `~/.config/sway/config.d/{wallpaper,cursor}.conf`.
- **`sway-scratch-show`** — Spawn-if-missing then toggle a scratchpad window by app_id. Caller passes the app_id and the spawn command; helper polls briefly for the window to map before issuing `scratchpad show`. Used by every scratchpad binding (`mod+'`, `mod+;`, `mod+g`, `mod+m`), so first press spawns and subsequent presses toggle. No autostart needed.

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
| `mod+f`                 | fullscreen toggle (native)   |
| `mod+j` / `mod+k`       | cycle (monocle-aware)        |
| `mod+h` / `mod+l`       | focus output left/right      |
| `mod+Shift+h/l`         | move container to L/R output |
| `mod+Shift+Return`      | sway-zoom-master             |
| `mod+t`                 | sway-float-toggle (centered) |
| `mod+q`                 | kill (native)                |
| `mod+1`–`mod+9`         | sway-ws switch (per-output)  |
| `mod+Shift+1`–`9`       | sway-ws move                 |
| `mod+apostrophe`        | scratchpad: TODO.md ↔ glances (monocle)    |
| `mod+semicolon`         | scratchpad: spotatui ↔ pulsemixer (monocle)|
| `mod+Shift+x`           | swaylock                     |
| `mod+Shift+w`           | wallpaper picker mode        |
| `mod+Shift+c`           | cursor picker mode           |
| `Print`                 | grim full-screen             |
| `mod+Shift+s`           | grim region → wl-copy        |

### Hyprland (experimental, side-by-side with sway)

The `hyprland/` stow package is a parallel scaffolding to test Hyprland as a sway alternative — primarily to recover kara's per-workspace dim+blur on scratchpads (Hyprland's "special workspaces" do this natively) and to get tunable animations. **Sway is not affected** — both packages live independently and start-sway / start-hyprland pick the session.

Layout:
- `hyprland/.config/hypr/hyprland.conf` — main config (master layout, per-output workspaces 1-9 via offset ranges, special-workspace scratchpads with dim+blur, snappy animations).
- `hyprland/.config/hypr/hyprpaper.conf` — wallpaper daemon config.
- `hyprland/.config/hypr/hypridle.conf` — idle orchestration (lock at 10m, DPMS at 15m, suspend at 30m).
- `hyprland/.config/hypr/hyprlock.conf` — lock screen (themed, blurred wallpaper backdrop).
- `hyprland/.config/swaync/{config.json,style.css}` — themed notification daemon + center.
- `hyprland/.config/fuzzel/fuzzel-hyprland.ini` — Hyprland-side fuzzel theme (rounded, translucent).
- `hyprland/.config/waybar/hyprland.jsonc` + `hyprland-style.css` — separate waybar config (pillier theme).
- `hyprland/.local/bin/start-hyprland` — TTY launcher.
- `hyprland/.local/bin/hyprland-monocle` — kara-style monocle via `fullscreenstate 1` (maximize). Bar/gaps/borders stay; cycle-next/prev atomically swap maximize state to the next leaf in one IPC call.
- `hyprland/.local/bin/hyprland-float-toggle` — mod+t replacement that sizes (~70x75%) and centers.

Same `.stow-local-ignore` workaround as sway: helpers must be `ln -sf`'d manually after stow.

To test, install Hyprland and the native ecosystem tools (all in the Artix `world`/`galaxy` repos — no AUR needed):
```
sudo pacman -S hyprland hyprpaper hypridle hyprlock hyprshot hyprpicker swaync
```
- `hyprshot` powers `Print` (output), `mod+Shift+s` (region), `mod+Print` (window).
- `hyprpicker` powers `mod+Shift+p` (copy color under cursor to clipboard).
(Also requires the existing sway dependencies — wpctl, grim, slurp, wl-clipboard, brightnessctl, playerctl.)
Then:
```
stow -t ~ hyprland
for f in ~/.dotfiles/hyprland/.local/bin/*; do ln -sf "$f" ~/.local/bin/$(basename "$f"); done
```
Log out, switch TTY, run `start-hyprland`. Logs at `~/.cache/hypr/start-hyprland.log` and Hyprland's own at `~/.local/share/hyprland/hyprland.log` (and a richer per-instance log in `/run/user/$UID/hypr/<sig>/`).

Not yet ported: hyprland-output-profile (docked/undocked monitor profile + hotplug — for now Hyprland's catch-all `monitor=,preferred,auto,1` plus the named overrides handle laptop-only cleanly; docked profile may need refinement), wallpaper/cursor pickers.

### Things kara had that sway doesn't replicate

- `sync_workspaces` toggle (`mod+s`) — sway has no concept; per-output independent is the only mode.
- Theme switcher (`mod+Shift+t`) — kara-beautify ran across many apps; needs a sway target in loom-rs to replicate.
- Keybind overlay (`mod+slash`) — sway has no built-in; could wire fuzzel-based later.
- Scratchpad dim/blur — kara dimmed and blurred the workspace underneath a scratchpad overlay. Sway can't do per-workspace visual effects. Since our scratchpad covers 100% of the output, there's nothing peeking through to dim — but the layered visual effect from kara-gate isn't replicable without patching the compositor.
- Multi-window scratchpads — kara packed multiple tiled windows into a single scratchpad. Sway's scratchpad pool is single-window; current setup splits each preset into its own scratchpad binding (TODO, glances, music, pulsemixer) rather than bundling.

### Stow notes

The `sway` package has a `.stow-local-ignore` excluding `.local` because the `scripts` package contains absolute-symlink entries (`claude`, `python3.11`) that stow globally refuses, and would abort the sibling-tree analysis when `sway/.local/bin/` is included. Workaround: `ln -sf` the helper scripts into `~/.local/bin/` directly. The bootstrap installer should do this after `stow_dotfiles`.

### Key Conventions

- XDG Base Directory spec is followed throughout
- Default editor: Neovim (nightly), terminal: kara-toe-client, browser: Floorp, shell: Zsh
- Neovim config is modular Lua with lazy.nvim plugin manager
- Makefile uses `>` as recipe prefix instead of TAB (`.RECIPEPREFIX := >`)
- Binaries built from source (loom, pmux) are gitignored; only wrapper scripts are tracked in `scripts/`
