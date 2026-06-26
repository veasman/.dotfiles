# Dotfiles

Personal dotfiles managed with **GNU Stow**. Each top-level directory is a stow package.
- Deploy: `stow -v -R -t "$HOME" <package>`
- Full install: `make install`
- Dry run: `make dry-run`

## Color palette (kara, fixed — no auto-theming)

bg `#111111`, surface `#1b1b1b`, text `#f2f2f2`, muted `#5c5c5c`
accent `#6bacac`, accent_soft `#458588`, border `#353535`
error `#bf616a`, warn `#ebcb8b`, success `#a3be8c`

## Visual rules

- Blur only on waybar layer (`layerrule = blur on, match:namespace waybar`). All other windows explicitly no_blur.
- Active window border: animated 4-stop gradient rotating on 5s loop. Slower is calmer.
- Notification body format: `Label <span muted>·</span> <span #a8a8a8>value</span>` (dim-dot pattern).
- Screenshot notifications: pre-rendered 200×125 letterboxed thumbnail (magick), click opens imv.
- Bar: height 30, font 10.5pt, pill padding 1×11, translucent bar bg 0.22 alpha, pills 0.92 alpha.

## Hyprland keybindings

- `mod+J` → cycle-next, `mod+K` → cycle-prev (j=down the win stack, k=up).
- Monocle: `fullscreenstate 1 -1` (maximize, NOT true fullscreen). True fullscreen is `mod+Shift+f` → `fullscreenstate 2 2`.
- `on_focus_under_fullscreen = 1` + `exit_window_retains_fullscreen = true` — both required for monocle to survive window events.
- `hyprctl reload` does NOT re-run `exec-once`. New autostart requires full restart.

## Window rules syntax (Hyprland 0.54+)

- `windowrule = match:FIELD VALUE, EFFECT [VALUE]` — space-separated match, NOT `field:regex`.
- Boolean effects need explicit value: `float on`, not bare `float`.
- Disable blur: `no_blur on` (underscore).

## Zsh perf

- NVM is lazy-loaded via shim functions. Don't revert to eager sourcing.
- `compinit` uses `-C` unless zcompdump is >24h old.
- `fast-syntax-highlighting` + `zsh-autosuggestions` are AUR packages (not yet in bootstrap installer).

## Default to Hyprland over Sway

- Hyprland is the active WM. Sway config exists but isn't the primary.
- Don't reintroduce kitty / yazi / sxiv (removed).
- No wallpaper-reactive theming (matugen, wallust, pywal). Palette is manual.

## Hermes stack (active LLM setup)

- `h`=FreeLLMAPI free, `hh`=DeepSeek Flash ($0.15/M), `hc`=Haiku code review ($1/M), `hhh`=Sonnet ($3/M), `hhhh`=Opus ($15/M).
- FreeLLMAPI at localhost:3001/v1 (Docker), OpenRouter primary, Ollama local (Qwen3-4B for sub-agents).
- Cognitive mode → model: scan=hh, review=hc, analyze=hhh, audit=hhhh, code=hh, decide=hhh/hhhh.
- No fallback_model (removed) — errors surface directly.

### Portable setup (new machine)

Two scripts total:
1. **`bootstrap/arch/pre-install.sh`** — run during arch-chroot (before first boot)
2. **`make install`** → `bootstrap/arch/install.sh` — run after first boot, handles everything:
   - Clones Hermes + installs into `~/.hermes/hermes-agent/venv/`
   - Deploys `~/.hermes/config.yaml`, wrapper scripts, aliases, desktop entry
   - Creates `~/.hermes/.env` from template and prompts for API keys
   - After install, verify with: `hermes model --list`
3. **(Optional)** — `hermes-setup --all` enables kanban dispatch + cron tick
