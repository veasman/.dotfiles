export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="foot"
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

# nvm global binaries — version is auto-managed by nvm, don't hardcode
export PATH="$NVM_DIR/versions/node/$(ls "$NVM_DIR/versions/node/" | sort -V | tail -1)/bin:$PATH"
