export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="kara-toe-client"
export BROWSER="floorp"

# Sway helpers live in the sway stow package (they can't go under
# scripts/.local/bin because the scripts package has absolute-symlink
# artifacts that abort stow). Add the dir to PATH directly so the
# helpers are reachable even if the manual symlinks in ~/.local/bin
# get reaped by a future stow re-run.
[ -d "$HOME/.dotfiles/sway/.local/bin" ] && \
    PATH="$HOME/.dotfiles/sway/.local/bin:$PATH"
