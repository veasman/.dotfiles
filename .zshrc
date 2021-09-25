export ZSH="/home/veasman/.local/src/oh-my-zsh"
ZSH_THEME="robbyrussell"
plugins=(git)
source $ZSH/oh-my-zsh.sh

setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments

# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=~/.cache/zsh/history

# Load alias and shortcuts
source ~/.config/shell/aliases.zsh
source ~/.config/shell/shortcuts.zsh

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char
bindkey '^[[P' delete-char

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# Add scripts to PATH
export PATH=/home/veasman/.local/bin:$PATH

# Lua LSP
alias luamake=/home/veasman/.local/bin/lua-language-server/3rd/luamake/luamake

# Load syntax highlighting
source /usr/share/zsh/site-contrib/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
