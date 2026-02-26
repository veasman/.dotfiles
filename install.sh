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
  exec 3> >(whiptail --title "Ubuntu dwm Installer" \
    --backtitle "Installing... Please wait" \
    --gauge "Preparing..." 12 80 0)
}

update_gui() {
  local percent="$1"
  local message="$2"
  echo "$percent" >&3
  echo "# $message" >&3
}

finish_gui() {
  update_gui 100 "Finalizing..."
  sleep 1
  exec 3>&-
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

  STEP=10

  if [[ "$selected" == *"packages"* ]]; then
    update_gui $STEP "Installing system packages..."
    install_packages
    STEP=30
  fi

  if [[ "$selected" == *"stow"* ]]; then
    update_gui $STEP "Stowing dotfiles..."
    stow_all
    STEP=50
  fi

  if [[ "$selected" == *"suckless"* ]]; then
    update_gui $STEP "Building dwm, dmenu, slstatus..."
    build_suckless
    STEP=70
  fi

  if [[ "$selected" == *"session"* ]]; then
    update_gui $STEP "Registering dwm session..."
    install_session
    STEP=85
  fi

  if [[ "$selected" == *"fonts"* ]]; then
    update_gui $STEP "Installing Fira Code..."
    install_fonts
    STEP=95
  fi

  if [[ "$selected" == *"shell"* ]]; then
    update_gui $STEP "Setting default shell..."
    set_shell
  fi

  finish_gui

  whiptail --title "Installation Complete" \
    --yesno "Ubuntu dwm setup finished successfully.\n\nWould you like to view the installation log?" 14 80

  if [[ $? -eq 0 ]]; then
    whiptail --title "Installation Log" --textbox "$LOG_FILE" 24 100
  fi

  whiptail --title "Next Steps" \
    --msgbox "To use dwm:\n\n1) Log out\n2) Select 'dwm' session\n3) Log in\n\nInstallation complete." 14 80
}

main