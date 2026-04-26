export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="kara-toe-client"
export BROWSER="floorp"

# ─── XDG base dirs ────────────────────────────────────────────────────
# Explicit values; some tools don't fall back to defaults. Cache is
# pointed under .local/cache to keep the home top-level lean (fewer
# entries in ~/).
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.local/cache"

# ─── Per-tool relocations under XDG ───────────────────────────────────
# Each of these moves ~/.foo into an XDG dir. The respective tool
# reads the env var on next invocation; existing data needs a one-
# shot `mv` to the new location (done at the same time as setting
# these env vars; see git history for the migration).
export CARGO_HOME="$XDG_CACHE_HOME/cargo"
export RUSTUP_HOME="$XDG_CACHE_HOME/rustup"
export NPM_CONFIG_CACHE="$XDG_CACHE_HOME/npm"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export GOPATH="$XDG_DATA_HOME/go"
export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export DOCKER_CONFIG="$XDG_CONFIG_HOME/docker"
export PYTHONHISTFILE="$XDG_STATE_HOME/python/history"
export LESSHISTFILE="$XDG_STATE_HOME/less/history"

# Sway helpers live in the sway stow package (they can't go under
# scripts/.local/bin because the scripts package has absolute-symlink
# artifacts that abort stow). Add the dir to PATH directly so the
# helpers are reachable even if the manual symlinks in ~/.local/bin
# get reaped by a future stow re-run.
[ -d "$HOME/.dotfiles/sway/.local/bin" ] && \
    PATH="$HOME/.dotfiles/sway/.local/bin:$PATH"
