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
# Only rebuild zcompdump if older than 24h; otherwise skip the security check
_zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"
if [[ -n "$_zcompdump"(#qN.mh+24) ]]; then
    compinit -d "$_zcompdump"
else
    compinit -C -d "$_zcompdump"
fi
unset _zcompdump

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

precmd_functions+=(_set_cursor_block)
_set_cursor_block

# Keybinds
bindkey -s '^f' 'pmux\n'

# PATH
export PATH="$HOME/.local/bin:/usr/local/go/bin:$PATH"

# NVM (lazy-loaded — sourcing nvm.sh costs 300-600ms, so defer until first use)
# NVM_DIR is set in .zshenv via $XDG_DATA_HOME; re-asserting here for clarity.
: "${NVM_DIR:=${XDG_DATA_HOME:-$HOME/.local/share}/nvm}"
export NVM_DIR

_nvm_load() {
    unset -f nvm node npm npx _nvm_load
    if [[ -s "/usr/share/nvm/init-nvm.sh" ]]; then
        source /usr/share/nvm/init-nvm.sh
    elif [[ -s "$NVM_DIR/nvm.sh" ]]; then
        source "$NVM_DIR/nvm.sh"
        [[ -s "$NVM_DIR/bash_completion" ]] && source "$NVM_DIR/bash_completion"
    fi
}
nvm()  { _nvm_load; nvm  "$@"; }
node() { _nvm_load; node "$@"; }
npm()  { _nvm_load; npm  "$@"; }
npx()  { _nvm_load; npx  "$@"; }

# Auto-use local node version when entering a project (lazy — only if .nvmrc present)
autoload -U add-zsh-hook
load-nvmrc() {
    [[ -f .nvmrc ]] || return
    _nvm_load
    local node_version="$(<.nvmrc)"
    local current_version="$(nvm version "$node_version")"
    if [[ "$current_version" = "N/A" ]]; then
        nvm install "$node_version"
    elif [[ "$(nvm current)" != "$current_version" ]]; then
        nvm use "$node_version" >/dev/null
    fi
}
add-zsh-hook chpwd load-nvmrc

# kara-beautify — fzf + session env vars
#
# The base we capture here must NOT contain any previously-injected
# --color= args — otherwise re-applies accumulate (shell A captures
# kara-injected colors as "base," child shell B then appends NEW
# kara colors on top of that "base," and fzf ends up with two
# --color= arg sets fighting each other). Strip every --color=...
# token out of the inherited FZF_DEFAULT_OPTS before storing, so
# BASE is strictly the user's non-color fzf options.
_kara_fzf_strip_colors() {
    local s="$1"
    local -a words=(${(z)s})
    local out="" w
    for w in $words; do
        [[ "$w" == --color=* ]] && continue
        out="${out:+$out }$w"
    done
    print -r -- "$out"
}

# Re-capture BASE on every shell init (unconditional, not `:-`), so a
# contaminated inherited FZF_DEFAULT_OPTS_BASE from a parent shell
# gets reset to the stripped baseline of whatever FZF_DEFAULT_OPTS
# currently holds.
export FZF_DEFAULT_OPTS_BASE="$(_kara_fzf_strip_colors "${FZF_DEFAULT_OPTS:-}")"
unfunction _kara_fzf_strip_colors

# Re-source kara's fzf + session env scripts when the file has been
# touched (or on first prompt after shell start). Lets existing
# shells pick up new colors from `kara-beautify apply` without a
# restart.
_kara_fzf_theme_path="${XDG_STATE_HOME:-$HOME/.local/state}/kara/generated/fzf-theme.sh"
_kara_session_theme_path="${XDG_STATE_HOME:-$HOME/.local/state}/kara/generated/session-theme.sh"
_kara_fzf_mtime=0

_kara_reload_fzf() {
    [ -f "$_kara_fzf_theme_path" ] || return 0
    local mtime
    mtime=$(stat -c %Y "$_kara_fzf_theme_path" 2>/dev/null) || return 0
    if [ "$_kara_fzf_mtime" != "$mtime" ]; then
        source "$_kara_fzf_theme_path"
        [ -f "$_kara_session_theme_path" ] && source "$_kara_session_theme_path"
        export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS_BASE:+$FZF_DEFAULT_OPTS_BASE }${KARA_FZF_COLOR_OPTS:-}"
        _kara_fzf_mtime="$mtime"
    fi
}

# Initial load at shell startup so the first fzf invocation has colors.
_kara_reload_fzf

# Hook into zsh's precmd so subsequent prompts re-check mtime and
# re-source if kara-beautify touched the file since last prompt.
autoload -Uz add-zsh-hook
add-zsh-hook precmd _kara_reload_fzf

# Plugins (load order matters: fast-syntax-highlighting BEFORE zsh-autosuggestions)
[[ -f /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh ]] && \
    source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
[[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]] && \
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

pfetch
#fastfetch
