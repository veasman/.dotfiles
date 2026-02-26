#!/usr/bin/env bash
set -Eeuo pipefail

DRY_RUN=0
[[ "${1:-}" == "--dry-run" ]] && DRY_RUN=1

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"

mkdir -p "$LOG_DIR"

require_ubuntu() {
  [[ -r /etc/os-release ]] || exit 1
  . /etc/os-release
  [[ "${ID:-}" == "ubuntu" || "${ID_LIKE:-}" == *"ubuntu"* || "${ID:-}" == "debian" ]] || {
    whiptail --title "Unsupported" --msgbox "Ubuntu/Debian only." 10 60
    exit 1
  }
}

ensure_sudo() {
  sudo -v
  ( while true; do sudo -n true; sleep 60; done ) 2>/dev/null &
  SUDO_KEEPALIVE_PID=$!
  trap 'kill "${SUDO_KEEPALIVE_PID:-0}" 2>/dev/null || true' EXIT
}

start_gui() {
  exec 3> >(whiptail --title "Ubuntu dwm Installer" --gauge "Starting..." 10 80 0)
}

update_gui() {
  local percent="$1"
  local message="$2"
  echo "$percent" >&3
  echo "# $message" >&3
}

run_cmd() {
  if [[ "$DRY_RUN" -eq 0 ]]; then
    "$@" >> "$LOG_FILE" 2>&1
  else
    echo "[DRY RUN] $*" >> "$LOG_FILE"
  fi
}

install_packages() {
  run_cmd sudo apt-get update -y
  run_cmd sudo apt-get upgrade -y

  run_cmd sudo apt-get install -y \
    stow git curl ca-certificates \
    build-essential pkg-config gcc make \
    zsh \
    ripgrep fd-find tmux fzf tree \
    xclip playerctl flameshot kitty \
    dunst libnotify-bin picom unclutter sxhkd \
    xwallpaper \
    wget unzip fontconfig \
    blueman light \
    libx11-dev libxinerama-dev libxft-dev \
    x11-xserver-utils dbus-x11 \
    xinit xserver-xorg-core
}

stow_all() {
  mkdir -p "$HOME/.config" "$HOME/.local/bin" "$HOME/code"
  run_cmd bash -c "cd '$DOTFILES_DIR' && stow -t '$HOME' shell nvim tmux kitty scripts wallpapers suckless xinit-desktop"
}

build_suckless() {
  for t in dwm dmenu slstatus; do
    run_cmd bash -c "cd '$HOME/code/$t' && make clean && make"
    run_cmd bash -c "cd '$HOME/code/$t' && sudo make install"
  done
}

install_session() {
  local dwm_bin="/usr/local/bin/dwm"
  [[ -x "$dwm_bin" ]] || dwm_bin="$(command -v dwm)"

  run_cmd sudo tee /usr/share/xsessions/dwm.desktop <<EOF
[Desktop Entry]
Name=dwm
Comment=Suckless dynamic window manager
Exec=$dwm_bin
Type=Application
EOF
}

install_fonts() {
  mkdir -p "$HOME/.local/share/fonts"
  tmp="$(mktemp -d)"
  run_cmd bash -c "
    cd '$tmp' &&
    curl -L -o Fira_Code.zip https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip &&
    unzip -q Fira_Code.zip &&
    cp -f ttf/*.ttf '$HOME/.local/share/fonts/'"
  rm -rf "$tmp"
  run_cmd fc-cache -f
}

set_shell() {
  [[ "${SHELL:-}" == "/bin/zsh" ]] || run_cmd chsh -s /bin/zsh "$USER"
}

main_menu() {
  whiptail --title "Ubuntu dwm Installer" \
    --checklist "Select installation steps:" 18 80 8 \
    "packages" "Install required packages" ON \
    "stow" "Stow dotfiles" ON \
    "suckless" "Build dwm/dmenu/slstatus" ON \
    "session" "Register dwm in login manager" ON \
    "fonts" "Install Fira Code" ON \
    "shell" "Set default shell to zsh" ON \
    3>&1 1>&2 2>&3
}

main() {
  require_ubuntu
  ensure_sudo

  selected="$(main_menu)" || exit 1

  start_gui

  [[ "$selected" == *"packages"* ]] && {
    update_gui 10 "Installing packages..."
    install_packages
  }

  [[ "$selected" == *"stow"* ]] && {
    update_gui 30 "Stowing dotfiles..."
    stow_all
  }

  [[ "$selected" == *"suckless"* ]] && {
    update_gui 55 "Building suckless tools..."
    build_suckless
  }

  [[ "$selected" == *"session"* ]] && {
    update_gui 75 "Registering dwm session..."
    install_session
  }

  [[ "$selected" == *"fonts"* ]] && {
    update_gui 85 "Installing fonts..."
    install_fonts
  }

  [[ "$selected" == *"shell"* ]] && {
    update_gui 95 "Setting default shell..."
    set_shell
  }

  update_gui 100 "Installation complete."
  sleep 1
  exec 3>&-

  whiptail --title "Done" \
    --msgbox "Installation complete.\n\nLog out and select 'dwm'.\n\nLog file:\n$LOG_FILE" 14 80
}

main