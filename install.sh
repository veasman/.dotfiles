#!/usr/bin/env bash
set -Eeuo pipefail

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"

mkdir -p "$LOG_DIR"

err() { printf "ERROR: %s\n" "$*" >&2; }
die() { err "$*"; exit 1; }
trap 'die "Failed at line $LINENO: $BASH_COMMAND (see $LOG_FILE)"' ERR

log() { printf "%s\n" "$*" | tee -a "$LOG_FILE" >/dev/null; }

require_ubuntu() {
  [[ -r /etc/os-release ]] || die "/etc/os-release not found."
  # shellcheck disable=SC1091
  . /etc/os-release
  if [[ "${ID:-}" != "ubuntu" && "${ID_LIKE:-}" != *"ubuntu"* && "${ID:-}" != "debian" ]]; then
    die "This installer supports Ubuntu/Debian-based systems only."
  fi
}

ensure_sudo() {
  command -v sudo >/dev/null 2>&1 || die "sudo not found."
  sudo -v
  ( while true; do sudo -n true; sleep 60; done ) 2>/dev/null &
  SUDO_KEEPALIVE_PID=$!
  trap 'kill "${SUDO_KEEPALIVE_PID:-0}" 2>/dev/null || true' EXIT
}

ensure_whiptail() {
  if command -v whiptail >/dev/null 2>&1; then return; fi
  sudo apt-get update -y | tee -a "$LOG_FILE"
  sudo apt-get install -y whiptail ca-certificates curl | tee -a "$LOG_FILE"
}

msgbox() {
  whiptail --title "$1" --msgbox "$2" 14 80
}

checklist() {
  whiptail --title "$1" --checklist "$2" "$3" "$4" "$5" "${@:6}" 3>&1 1>&2 2>&3
}

radiolist() {
  whiptail --title "$1" --radiolist "$2" "$3" "$4" "$5" "${@:6}" 3>&1 1>&2 2>&3
}

ensure_dirs() {
  mkdir -p "$HOME/.config" "$HOME/.local/bin" "$HOME/code"
}

stow_modules() {
  [[ -d "$DOTFILES_DIR" ]] || die "Dotfiles dir not found: $DOTFILES_DIR"
  (cd "$DOTFILES_DIR" && stow -t "$HOME" "$@") 2>&1 | tee -a "$LOG_FILE"
}

install_base_packages() {
  sudo apt-get update -y | tee -a "$LOG_FILE"
  sudo apt-get upgrade -y | tee -a "$LOG_FILE"

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
    | tee -a "$LOG_FILE"
}

install_startx_stack() {
  sudo apt-get install -y \
    xinit \
    xserver-xorg-core \
    | tee -a "$LOG_FILE"
}

install_fonts() {
  mkdir -p "$HOME/.local/share/fonts"
  tmp="$(mktemp -d)"
  (
    cd "$tmp"
    curl -L -o Fira_Code.zip https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip
    unzip -q Fira_Code.zip
    cp -f ttf/*.ttf "$HOME/.local/share/fonts/"
  ) 2>&1 | tee -a "$LOG_FILE"
  rm -rf "$tmp"
  fc-cache -f >/dev/null 2>&1 || true
}

build_install_suckless() {
  local base="$HOME/code"
  local targets=(dwm dmenu slstatus)

  for t in "${targets[@]}"; do
    [[ -d "$base/$t" ]] || die "Missing $base/$t. Did you stow 'suckless'?"
  done

  for t in "${targets[@]}"; do
    log "Building $t..."
    (cd "$base/$t" && make clean && make) 2>&1 | tee -a "$LOG_FILE"
    log "Installing $t..."
    (cd "$base/$t" && sudo make install) 2>&1 | tee -a "$LOG_FILE"
  done
}

install_dwm_session() {
  local xsessions="/usr/share/xsessions"
  [[ -d "$xsessions" ]] || { log "No $xsessions found. Skipping session registration."; return; }

  local dwm_bin="/usr/local/bin/dwm"
  [[ -x "$dwm_bin" ]] || dwm_bin="$(command -v dwm || true)"
  [[ -n "${dwm_bin:-}" ]] || die "dwm not found."

  sudo tee "$xsessions/dwm.desktop" >/dev/null <<EOF
[Desktop Entry]
Name=dwm
Comment=Suckless dynamic window manager
Exec=$dwm_bin
Type=Application
EOF
}

set_zsh_default() {
  [[ "${SHELL:-}" == "/bin/zsh" ]] && return
  chsh -s /bin/zsh "$USER" || true
}

pick_xinit() {
  radiolist \
    "Xinit (.xinitrc)" \
    "Only choose if you use startx. If you log in via GDM, choose Skip." \
    14 80 3 \
    "skip" "Do not install startx stack or .xinitrc" ON \
    "xinit-desktop" "Install startx stack + stow Ubuntu .xinitrc" OFF \
    "xinit-gentoo" "Stow Gentoo placeholder (unused)" OFF
}

main_menu() {
  checklist \
    "Ubuntu dwm Installer" \
    "Select what to run:" \
    18 90 8 \
    "packages" "Install all required packages" ON \
    "stow" "Stow dotfiles into HOME" ON \
    "suckless" "Build+install dwm+dmenu+slstatus" ON \
    "session" "Register dwm in login manager (GDM)" ON \
    "fonts" "Install Fira Code" ON \
    "shell" "Set default shell to zsh" OFF
}

main() {
  require_ubuntu
  ensure_sudo
  ensure_whiptail
  ensure_dirs

  msgbox "Ubuntu dwm Installer" "Ubuntu-only.\n\nLog file:\n$LOG_FILE"

  selected="$(main_menu)" || exit 1

  xinit_choice=""
  if grep -q "\"stow\"" <<<"$selected"; then
    xinit_choice="$(pick_xinit)"
  fi

  if grep -q "\"packages\"" <<<"$selected"; then install_base_packages; fi

  if [[ "$xinit_choice" == "xinit-desktop" ]]; then
    install_startx_stack
  fi

  if grep -q "\"stow\"" <<<"$selected"; then
    modules=(shell nvim tmux kitty scripts wallpapers suckless)
    [[ "$xinit_choice" == "xinit-desktop" ]] && modules+=(xinit-desktop)
    [[ "$xinit_choice" == "xinit-gentoo" ]] && modules+=(xinit-gentoo)
    stow_modules "${modules[@]}"
  fi

  if grep -q "\"fonts\"" <<<"$selected"; then install_fonts; fi
  if grep -q "\"suckless\"" <<<"$selected"; then build_install_suckless; fi
  if grep -q "\"session\"" <<<"$selected"; then install_dwm_session; fi
  if grep -q "\"shell\"" <<<"$selected"; then set_zsh_default; fi

  msgbox "Done" \
    "Installation complete.\n\nTo use dwm:\n1) Log out\n2) Select 'dwm' session\n3) Log in\n\nLog:\n$LOG_FILE"
}

main