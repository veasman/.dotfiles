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

The bootstrap installer deploys these packages: floorp, fonts, fuzzel, git, gtk, hyprland, latex, nvim, pmux, scripts, shell, tmux, waybar, xdg.

### Bootstrap System

- `bootstrap/arch/pre-install.sh` — Phase 1: runs inside `artix-chroot`, creates user, generates SSH key, clones dotfiles
- `bootstrap/arch/install.sh` — Phase 2: post-reboot installer (interactive whiptail dialogs, supports `--dry-run`). Uses paru (AUR helper) and sigil (replaces loom for theme compilation)

### Custom Tools

- **pmux / pmux-run / pmux-cheat** — Project multiplexer for tmux sessions.
- **kara-toe-client** — Foot-fork terminal vendored at `~/repos/kara/crates/kara-toe/` and installed via `kara` repo. Default `$TERMINAL` system-wide.

### Output topology

Three monitors when docked, laptop alone when not:
- `DVI-I-2` — left portrait (1920×1080, transform 1 = 90°, position 0,0). On evdi (DisplayLink).
- `DVI-I-1` — center 1440p (2560×1440@144Hz, position 1080,0). The primary work area.
- `DP-2` — right portrait (1920×1080, transform 3 = 270°, position 3640,0). Direct on amdgpu.
- `eDP-1` — laptop panel; only enabled when externals are absent.

### Keybind quick-reference

| key                     | action                                           |
|-------------------------|--------------------------------------------------|
| `mod+Return`            | terminal (kara-toe-client)                       |
| `mod+d`                 | fuzzel launcher                                  |
| `mod+e`                 | lf in kara-toe-client                            |
| `mod+f`                 | hyprland-monocle toggle                          |
| `mod+Shift+f`           | true fullscreen                                  |
| `mod+j` / `mod+k`       | hyprland-monocle cycle (J=prev, K=next)          |
| `mod+h` / `mod+l`       | hyprland-output focus L/R (wraps)                |
| `mod+Shift+h` / `+l`    | hyprland-output move + follow                    |
| `mod+Shift+Return`      | layoutmsg swapwithmaster                         |
| `mod+t`                 | hyprland-float-toggle                            |
| `mod+q`                 | killactive                                       |
| `mod+1`–`mod+9`         | hyprland-ws switch (per-output)                  |
| `mod+Shift+1`–`9`       | hyprland-ws move                                 |
| `mod+apostrophe`        | scratchpad: TODO.md ↔ glances                    |
| `mod+semicolon`         | scratchpad: spotatui ↔ pulsemixer                |
| `mod+m`                 | scratchpad: floorp-mail (lazy spawn)             |
| `mod+Shift+w`           | wallpaper picker submap                          |
| `mod+Shift+c`           | cursor picker submap                             |
| `mod+Shift+x`           | hyprlock                                         |
| `Print`                 | hyprshot output                                  |
| `mod+Shift+s`           | hyprshot region                                  |
| `mod+Print`             | hyprshot window                                  |
| `mod+Shift+p`           | hyprpicker color → wl-copy                       |

### Hyprland

The `hyprland/` stow package is the only session — there is no fallback compositor in this repo. Launched from the TTY via `start-hyprland`.

Layout:
- `hyprland/.config/hypr/hyprland.conf` — main config (master layout, per-output workspace ranges, special-workspace scratchpads with dim+blur, cursor warp on workspace change + monitor focus, snappy animations).
- `hyprland/.config/hypr/hyprpaper.conf` — wallpaper daemon config.
- `hyprland/.config/hypr/hypridle.conf` — idle orchestration (lock at 10m, DPMS at 15m, suspend at 30m).
- `hyprland/.config/hypr/hyprlock.conf` — lock screen (themed, blurred wallpaper backdrop).
- `hyprland/.config/swaync/{config.json,style.css}` — themed notification daemon + center.
- `hyprland/.config/swayosd/` — volume/brightness HUD config (now used by the `volume`/`brightness` wrappers; replaces the swaync notification-as-OSD pattern that lived earlier in the sprint).
- `hyprland/.config/fuzzel/fuzzel-hyprland.ini` — Hyprland-side fuzzel theme (rounded, translucent).
- `hyprland/.config/waybar/hyprland.jsonc` + `hyprland-style.css` — separate waybar config (pillier theme, gruvbox hard).
- `hyprland/.local/bin/start-hyprland` — TTY launcher.
- `hyprland/.local/bin/hyprland-monocle` — kara-style monocle via `fullscreenstate 1` (maximize). Bar/gaps/borders stay; cycle-next/prev atomically swap maximize state to the next leaf in one IPC call. **Bind direction**: `mod+J` → `cycle-prev`, `mod+K` → `cycle-next` (the user's "j is down the stack, k is up the stack" — *do not* re-swap without re-testing).
- `hyprland/.local/bin/hyprland-float-toggle` — mod+t replacement that sizes (~70x75%) and centers.
- `hyprland/.local/bin/hyprland-wallpaper` — preview-cycle wallpaper picker driven by the `wallpaper` submap (mod+Shift+w). Pushes via `hyprctl hyprpaper`; persists selection to `~/.local/state/hypr/wallpaper-current`.
- `hyprland/.local/bin/hyprland-cursor` — analogous cursor-theme picker driven by the `cursor` submap (mod+Shift+c). Pushes via `hyprctl setcursor`; persists to `~/.local/state/hypr/cursor-current`.
- `hyprland/.local/bin/hyprland-output-profile` — docked vs. undocked monitor profile + hot-plug handler. Detects `DVI-I-1`/`DVI-I-2`/`DP-2` presence at startup and applies the dock layout (DVI-I-2 left portrait `transform 1`, DVI-I-1 1440p center, DP-2 right portrait `transform 3`, eDP-1 disabled); falls back to eDP-1-only when undocked. Snaps focus to DVI-I-1 in docked mode so autostarts (floorp) and any toggled scratchpad land on the main work area. Subscribes to Hyprland's `socket2` event stream for `monitoradded`/`monitorremoved` so plug/unplug re-applies automatically. Single-instance via `flock` on `${XDG_RUNTIME_DIR}/hyprland-output-profile.lock`.
- `hyprland/.local/bin/hyprland-ws` — per-output workspace switcher. `mod+1..9` / `mod+Shift+1..9` translate to absolute IDs based on focused monitor (eDP-1=1-9, DVI-I-2=11-19, DVI-I-1=21-29, DP-2=31-39). Workspaces are created **on demand** (we tried `persistent:true` and rolled back — when a bound monitor was disabled, the persistent workspaces fell back onto whichever monitor was active and duplicated the per-output digits in the bar). Waybar's `format-icons` map collapses each range to its visible digit so every monitor reads as 1-9.
- `hyprland/.local/bin/hyprland-output` — spatial cross-monitor focus / move with wraparound. `mod+H/L` and `mod+Shift+H/L` go through here instead of the bare `focusmonitor l/r`, which doesn't wrap on three monitors. Also issues a `movecursor` to the destination monitor's center so keyboard focus and pointer stay together (Hyprland's `cursor:warp_on_change_workspace` handles in-monitor switches but `focusmonitor` itself doesn't warp).
- `hyprland/.local/bin/hyprland-ws-migrate` — one-shot cleanup. Walks every legacy global ws 1-9 that still has windows, looks up each one's current monitor, and silently rehomes its clients to the per-output equivalent (e.g. ws 2 on DVI-I-1 → ws 22). Idempotent. Useful after switching from Hyprland's native global workspaces to per-output ranges, or after any session where output-profile dispatched a workspace from the wrong focus context.

Same `.stow-local-ignore` workaround as sway: helpers must be `ln -sf`'d manually after stow.

#### Theme

Gruvbox hard contrast (`bg = #1d2021`) is the canonical palette and is hand-maintained — `kara-beautify` / `loom-rs` auto-theming is **off**. The five files holding it:
- `~/.config/kara/kara-toe.ini` — `[colors-dark]` block with `background=1d2021`.
- `~/.local/state/kara/generated/tmux-theme.conf` — sourced by `tmux/.config/tmux/tmux.conf` if-exists.
- `~/.local/state/kara/generated/nvim-theme.lua` — read by `kara.nvim` (auto-reload watches the file).
- `~/.local/state/kara/generated/fzf-theme.sh` — sourced by `shell/.zshrc` into `FZF_DEFAULT_OPTS`.
- `hyprland/.config/waybar/hyprland-style.css` — bar accents.

If you ever re-enable kara-beautify, expect it to overwrite the four state files; `[consumers]` flags in `~/.config/kara/kara-beautify.toml` opt individual targets out.

#### File picker + screencast picker

Two portal interfaces are routed away from the GTK default:
- **FileChooser** → `xdg-desktop-portal-termfilechooser-hunkyburrito`. Config in `~/.config/xdg-desktop-portal-termfilechooser/config` points at `lf-wrapper.sh`, which spawns lf in `kara-toe-client --app-id=lf-portal`; a windowrule in `hyprland.conf` makes that app_id float at 75%×80% centered. lf's preview pane uses chafa (already in `~/.config/lf/preview`) so receipt-photo previews are vastly larger than the GTK chooser's 96px thumbnails.
- **ScreenCast** → `xdg-desktop-portal-hyprland` with `hyprland-share-picker-preview-git` (AUR) replacing the bundled picker. Visual Qt UI showing every window / monitor as a thumbnail — used by MS Teams, Zoom, OBS' pipewire source, etc. Replaces the wlr backend's text-only output-name list.

Both are wired in `~/.config/xdg-desktop-portal/portals.conf` (stowed via the `gtk` package).

To test, install Hyprland and the native ecosystem tools:
```
sudo pacman -S hyprland hyprpaper hypridle hyprlock hyprshot hyprpicker swaync swayosd socat
paru -S xdg-desktop-portal-termfilechooser-hunkyburrito-git
```
- `hyprshot` powers `Print` (output), `mod+Shift+s` (region), `mod+Print` (window).
- `hyprpicker` powers `mod+Shift+p` (copy color under cursor to clipboard).
- `socat` is required by `hyprland-output-profile` for the Hyprland event subscription socket.
- `swayosd-server` is a tiny centered HUD daemon; `volume` / `brightness` wrappers in `scripts/.local/bin/` delegate to it when present.
(Also requires wpctl, grim, slurp, wl-clipboard, brightnessctl, playerctl — all in the bootstrap install list.)
Then:
```
stow -t ~ hyprland
for f in ~/.dotfiles/hyprland/.local/bin/*; do ln -sf "$f" ~/.local/bin/$(basename "$f"); done
```
Log out, switch TTY, run `start-hyprland`. Logs at `~/.cache/hypr/start-hyprland.log` and Hyprland's own at `~/.local/share/hyprland/hyprland.log` (and a richer per-instance log in `/run/user/$UID/hypr/<sig>/`).

### Stow notes

The `hyprland` package has a `.stow-local-ignore` excluding `.local` because the `scripts` package contains absolute-symlink entries (`claude`, `python3.11`) that stow globally refuses — including `.local/bin/` aborts sibling-tree analysis. Workaround: `ln -sf` the helper scripts into `~/.local/bin/` directly after `stow_dotfiles`. The bootstrap installer's `stow_dotfiles()` does this for `hyprland/.local/bin/*`.

### Key Conventions

- XDG Base Directory spec is followed throughout
- Default editor: Neovim (nightly), terminal: kara-toe-client, browser: Floorp, shell: Zsh
- Neovim config is modular Lua with lazy.nvim plugin manager
- Makefile uses `>` as recipe prefix instead of TAB (`.RECIPEPREFIX := >`)
- Binaries built from source (loom, pmux) are gitignored; only wrapper scripts are tracked in `scripts/`
