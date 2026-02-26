#!/usr/bin/env bash
set -Eeuo pipefail

DRY_RUN=0
[[ "${1:-}" == "--dry-run" ]] && DRY_RUN=1

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"
mkdir -p "$LOG_DIR"

exec 3>&1

log() { printf "%s\n" "$*" >> "$LOG_FILE"; }

die() {
  whiptail --title "Error" --msgbox "$1\n\nLog:\n$LOG_FILE" 14 80
  exit 1
}

require_ubuntu() {
  [[ -r /etc/os-release ]] || die "/etc/os-release not found."
  . /etc/os-release
  if [[ "${ID:-}" != "ubuntu" && "${ID_LIKE:-}" != *"ubuntu"* && "${ID:-}" != "debian" ]]; then
    die "Unsupported distro. Ubuntu/Debian only."
  fi
}

ensure_sudo() {
  command -v sudo >/dev/null 2>&1 || die "sudo not found."
  sudo -v
  ( while true; do sudo -n true; sleep 60; done ) 2>/dev/null &
  SUDO_KEEPALIVE_PID=$!
  trap 'kill "${SUDO_KEEPALIVE_PID:-0}" 2>/dev/null || true' EXIT
}

run_step() {
  local percent="$1"
  local message="$2"
  shift 2

  {
    echo "$percent"
    echo "# $message"
  } | whiptail --gauge "Ubuntu dwm Installer" 8 78 "$percent" &

  if [[ "$DRY_RUN" -eq 0 ]]; then
    "$@" >> "$LOG_FILE" 2>&1
  else
    echo "[DRY RUN] $*" >> "$LOG_FILE"
  fi
}

install_packages() {
  sudo apt-get update -y >> "$LOG_FILE" 2>&1
  sudo apt-get upgrade -y >> "$LOG_FILE" 2>&1

  sudo apt-get install -y \
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
    x11-xserver-utils \
    dbus-x11 \
    xinit xserver-xorg-core \
    >> "$LOG_FILE" 2>&1
}

stow_all() {
  mkdir -p "$HOME/.config" "$HOME/.local/bin" "$HOME/code"
  (cd "$DOTFILES_DIR" && stow -t "$HOME" shell nvim tmux kitty scripts wallpapers suckless xinit-desktop) >> "$LOG_FILE" 2>&1
}

build_suckless() {
  for t in dwm dmenu slstatus; do
    (cd "$HOME/code/$t" && make clean && make) >> "$LOG_FILE" 2>&1
    (cd "$HOME/code/$t" && sudo make install) >> "$LOG_FILE" 2>&1
  done
}

install_session() {
  local xsessions="/usr/share/xsessions"
  local dwm_bin="/usr/local/bin/dwm"

  [[ -x "$dwm_bin" ]] || dwm_bin="$(command -v dwm)"

  sudo tee "$xsessions/dwm.desktop" >/dev/null <<EOF
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
  (
    cd "$tmp"
    curl -L -o Fira_Code.zip https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip
    unzip -q Fira_Code.zip
    cp -f ttf/*.ttf "$HOME/.local/share/fonts/"
  ) >> "$LOG_FILE" 2>&1
  rm -rf "$tmp"
  fc-cache -f >> "$LOG_FILE" 2>&1
}

set_shell() {
  [[ "${SHELL:-}" == "/bin/zsh" ]] && return
  chsh -s /bin/zsh "$USER" >> "$LOG_FILE" 2>&1 || true
}

main_menu() {
  whiptail --title "Ubuntu dwm Installer" \
    --checklist "Select installation steps:" 18 80 8 \
    "packages" "Install all required packages" ON \
    "stow" "Stow dotfiles" ON \
    "suckless" "Build and install dwm/dmenu/slstatus" ON \
    "session" "Register dwm in login manager" ON \
    "fonts" "Install Fira Code" ON \
    "shell" "Set default shell to zsh" ON \
    3>&1 1>&2 2>&3
}

main() {
  require_ubuntu
  ensure_sudo

  whiptail --title "Ubuntu dwm Installer" \
    --msgbox "Ubuntu-only dwm setup.\n\nDry Run Mode: $([[ "$DRY_RUN" -eq 1 ]] && echo ENABLED || echo DISABLED)\n\nLog file:\n$LOG_FILE" 14 80

  selected="$(main_menu)" || exit 1

  [[ "$selected" == *"packages"* ]] && run_step 10 "Installing packages..." install_packages
  [[ "$selected" == *"stow"* ]] && run_step 30 "Stowing dotfiles..." stow_all
  [[ "$selected" == *"suckless"* ]] && run_step 60 "Building suckless tools..." build_suckless
  [[ "$selected" == *"session"* ]] && run_step 80 "Registering dwm session..." install_session
  [[ "$selected" == *"fonts"* ]] && run_step 90 "Installing fonts..." install_fonts
  [[ "$selected" == *"shell"* ]] && run_step 95 "Setting default shell..." set_shell

  whiptail --title "Done" \
    --msgbox "Installation complete.\n\nLog out and select 'dwm' session.\n\nLog:\n$LOG_FILE" 14 80
}

main