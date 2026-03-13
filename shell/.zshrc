# Enable colors and change prompt
#autoload -U colors && colors
#PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' use-prompt-escapes yes
zstyle ':vcs_info:git:*' formats ' %F{magenta}git:(%F{red}%b%F{magenta})%f'
zstyle ':vcs_info:git:*' actionformats ' %F{magenta}git:(%F{red}%b%F{yellow}|%a%F{magenta})%f'

prompt_git_dirty() {
    git rev-parse --is-inside-work-tree >/dev/null 2>&1 || return

    if ! git diff --no-ext-diff --quiet 2>/dev/null || ! git diff --no-ext-diff --cached --quiet 2>/dev/null; then
        print -r -- ' %F{yellow}✗%f'
    fi
}

precmd_update_prompt() {
    vcs_info
}

precmd_functions+=(precmd_update_prompt)

PROMPT='%B%F{green}➜ %F{cyan}%c${vcs_info_msg_0_}$(prompt_git_dirty) %f%b'

# Behavior
setopt autocd
setopt interactive_comments
setopt appendhistory
setopt sharehistory
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt extendedhistory

# Disable Ctrl-S to freeze terminal
[[ -t 0 ]] && stty stop undef

HISTSIZE=200000
SAVEHIST=200000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"

# Ensure history dir exists
mkdir -p "${HISTFILE:h}"

# Load aliases and shortcuts
[[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc" ]] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/shortcutrc"
[[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ]] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc" ]] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/zshnameddirrc"

# Completion
autoload -Uz compinit
zmodload zsh/complist

: ${XDG_CACHE_HOME:=$HOME/.cache}
mkdir -p "$XDG_CACHE_HOME/zsh"
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump"

zstyle ':completion:*' menu select
zstyle ':completion:*' special-dirs false
zstyle ':completion:*' matcher-list \
    'm:{a-zA-Z}={A-Za-z}' \
    'r:|[._-]=* r:|=*' \
    'l:|=* r:|=*'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{yellow}%d%f'
zstyle ':completion:*' list-separator ' — '
zstyle ':completion:*' file-sort name
zstyle ':completion:*' squeeze-slashes true

# Vim mode
bindkey -v
export KEYTIMEOUT=1

# Vim keys in completion menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# Backspace / delete
bindkey -v '^?' backward-delete-char
# ^[[P is the original
# bindkey '^[[3~' delete-char
bindkey '^[[P' delete-char
bindkey -M vicmd '^[[3~' vi-delete-char
bindkey -M visual '^[[3~' vi-delete

# Edit command line in vim
autoload edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line
bindkey -M vicmd '^e' edit-command-line

# Cursor shape
function _set_cursor_block() {
    echo -ne '\e[1 q'
}

function _set_cursor_beam() {
    echo -ne '\e[5 q'
}

function zle-keymap-select () {
    case $KEYMAP in
        vicmd) _set_cursor_block ;;
        viins|main) _set_cursor_block ;;
    esac
}
zle -N zle-keymap-select

zle-line-init() {
    zle -K viins
    _set_cursor_block
}
zle -N zle-line-init

# Old way
# preexec() { _set_cursor_block }
# New way
precmd_functions+=(_set_cursor_block)

# Startup cursor
_set_cursor_block

# Keybinds
bindkey -s '^f' 'pmux\n'

# PATH
export PATH="$HOME/.local/bin:/usr/local/go/bin:$PATH"

# NVM
export NVM_DIR="$HOME/.nvm"
[[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh"

# Auto-use local node version when entering a project
autoload -U add-zsh-hook

load-nvmrc() {
    local nvmrc_path
    nvmrc_path="$(nvm_find_nvmrc 2>/dev/null)"

    if [[ -n "$nvmrc_path" ]]; then
        local node_version
        node_version="$(<"$nvmrc_path")"
        local current_version
        current_version="$(nvm version "$node_version")"

        if [[ "$current_version" = "N/A" ]]; then
            nvm install "$node_version"
        elif [[ "$(nvm current)" != "$current_version" ]]; then
            nvm use "$node_version" >/dev/null
        fi
    fi
}

add-zsh-hook chpwd load-nvmrc
load-nvmrc

# LOOM

# fzf config
[ -f "${XDG_STATE_HOME:-$HOME/.local/state}/loom/generated/fzf-theme.sh" ] && source "${XDG_STATE_HOME:-$HOME/.local/state}/loom/generated/fzf-theme.sh"
export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS:+$FZF_DEFAULT_OPTS }${LOOM_FZF_COLOR_OPTS:-}"
