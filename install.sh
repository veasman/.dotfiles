#!/usr/bin/env bash
set -Eeuo pipefail

DRY_RUN=0
if [[ "${1:-}" == "--dry-run" ]]; then
  DRY_RUN=1
fi

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"
mkdir -p "$LOG_DIR"

# Keep whiptail stdout usable.
exec 3>&1

# ----------------------------
# Helpers
# ----------------------------
log() { printf "%s\n" "$*" >> "$LOG_FILE"; }

die_ui() {
  local msg="$1"
  # Close gauge if open
  if [[ -n "${GAUGE_FD_OPEN:-}" ]]; then
    exec 4>&- || true
    GAUGE_FD_OPEN=""
  fi
  whiptail --title "Error" --msgbox "$msg\n\nLog:\n$LOG_FILE" 14 84
  if whiptail --title "View Log?" --yesno "Open the log now?" 10 60; then
    whiptail --title "Install Log" --textbox "$LOG_FILE" 28 110
  fi
  exit 1
}

trap 'die_ui "Failed at line $LINENO:\n$BASH_COMMAND\n\nLast log lines:\n$(tail -n 80 "$LOG_FILE" 2>/dev/null || true)"' ERR

require_ubuntu() {
  [[ -r /etc/os-release ]] || die_ui "/etc/os-release not found."
  # shellcheck disable=SC1091
  . /etc/os-release
  if [[ "${ID:-}" != "ubuntu" && "${ID:-}" != "debian" && "${ID_LIKE:-}" != *"ubuntu"* && "${ID_LIKE:-}" != *"debian"* ]]; then
    die_ui "Unsupported distro. Ubuntu/Debian-based only."
  fi
}

ensure_sudo() {
  command -v sudo >/dev/null 2>&1 || die_ui "sudo not found."
  # You cannot avoid the sudo prompt without stealing the password. So we do it upfront.
  whiptail --title "Privileges" --msgbox "Next, sudo may ask for your password in the terminal.\n\nThis is expected.\n\nPress OK, then enter your password if prompted." 12 80
  sudo -v >>"$LOG_FILE" 2>&1
  ( while true; do sudo -n true; sleep 60; done ) 2>/dev/null &
  SUDO_KEEPALIVE_PID=$!
  trap 'kill "${SUDO_KEEPALIVE_PID:-0}" 2>/dev/null || true' EXIT
}

ensure_whiptail() {
  if command -v whiptail >/dev/null 2>&1; then return; fi
  sudo apt-get update -y >>"$LOG_FILE" 2>&1
  sudo apt-get install -y whiptail ca-certificates curl >>"$LOG_FILE" 2>&1
}

# ----------------------------
# Persistent Gauge
# ----------------------------
start_gauge() {
  # FD 4 feeds the gauge. Keep it open for the whole install.
  exec 4> >(whiptail --title "Ubuntu dwm Installer" \
    --backtitle "Installing… (log: ~/.local/state/dotfiles-installer/install.log)" \
    --gauge "Preparing…" 12 90 0)
  GAUGE_FD_OPEN=1
}

gauge() {
  local pct="$1"
  local msg="$2"
  echo "$pct" >&4
  echo "# $msg" >&4
}

finish_gauge() {
  gauge 100 "Finalizing…"
  sleep 1
  exec 4>&-
  GAUGE_FD_OPEN=""
}

run_cmd() {
  if [[ "$DRY_RUN" -eq 1 ]]; then
    log "[DRY RUN] $*"
    return 0
  fi
  "$@" >>"$LOG_FILE" 2>&1
}

run_pipe_to_file() {
  # usage: run_pipe_to_file "https://…" "/path/to/file"
  local url="$1" out="$2"
  if [[ "$DRY_RUN" -eq 1 ]]; then
    log "[DRY RUN] curl -fsSL '$url' > '$out'"
    return 0
  fi
  curl -fsSL "$url" | sudo tee "$out" >/dev/null 2>>"$LOG_FILE"
}

run_sh() {
  # Convenience for complex commands
  if [[ "$DRY_RUN" -eq 1 ]]; then
    log "[DRY RUN] bash -c $1"
    return 0
  fi
  bash -c "$1" >>"$LOG_FILE" 2>&1
}

# ----------------------------
# Actions
# ----------------------------
install_base_packages() {
  run_cmd sudo apt-get update -y
  run_cmd sudo apt-get upgrade -y

  # Core tooling + your environment + suckless build deps + startx support
  # NOTE: Keeping picom (not xcompmgr).
  run_cmd sudo apt-get install -y \
    stow git curl ca-certificates wget unzip fontconfig \
    build-essential pkg-config gcc make \
    zsh tmux fzf tree ripgrep fd-find \
    xclip playerctl flameshot kitty \
    dunst libnotify-bin picom unclutter sxhkd xwallpaper \
    blueman light \
    gnupg \
    libx11-dev libxinerama-dev libxft-dev \
    x11-xserver-utils dbus-x11 \
    xinit xserver-xorg-core
}

stow_dotfiles() {
  [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles dir not found: $DOTFILES_DIR"
  run_cmd mkdir -p "$HOME/.config" "$HOME/.local/bin" "$HOME/code"

  local modules=(shell nvim tmux kitty scripts wallpapers suckless xinit-desktop)

  # Preflight dry-run to detect conflicts
  local tmp
  tmp="$(mktemp)"
  if ! (cd "$DOTFILES_DIR" && stow -n -v -t "$HOME" "${modules[@]}") >"$tmp" 2>&1; then
    # Conflicts or other stow issue
    whiptail --title "Stow Conflicts Detected" --msgbox \
"Stow reports conflicts (existing files in \$HOME).\n\nYou must choose what to do next." 12 80

    if whiptail --title "Conflict Details" --yesno "View conflict output now?" 10 60; then
      whiptail --title "Stow Output" --textbox "$tmp" 28 110
    fi

    if whiptail --title "Resolve Conflicts" --yesno \
"Option A (SAFE): Abort and resolve conflicts manually.\n\nOption B (RISKY): Use --adopt to take existing files into the repo (can overwrite/move files).\n\nUse --adopt?" 16 80; then
      # adopt
      (cd "$DOTFILES_DIR" && run_cmd stow --adopt -v -t "$HOME" "${modules[@]}")
    else
      rm -f "$tmp"
      die_ui "Aborted due to stow conflicts. Resolve conflicts and re-run."
    fi
  else
    # No conflicts, do the real stow
    rm -f "$tmp"
    (cd "$DOTFILES_DIR" && run_cmd stow -v -t "$HOME" "${modules[@]}")
  fi
}

build_install_suckless() {
  for t in dwm dmenu slstatus; do
    [[ -d "$HOME/code/$t" ]] || die_ui "Missing $HOME/code/$t. Stow 'suckless' first."
    run_sh "cd '$HOME/code/$t' && make clean && make"
    run_sh "cd '$HOME/code/$t' && sudo make install"
  done
}

register_dwm_session() {
  local xsessions="/usr/share/xsessions"
  if [[ ! -d "$xsessions" ]]; then
    log "No $xsessions found; skipping session registration."
    return 0
  fi

  local dwm_bin="/usr/local/bin/dwm"
  if [[ ! -x "$dwm_bin" ]]; then
    dwm_bin="$(command -v dwm || true)"
  fi
  [[ -n "${dwm_bin:-}" ]] || die_ui "dwm not found after install."

  if [[ "$DRY_RUN" -eq 1 ]]; then
    log "[DRY RUN] write $xsessions/dwm.desktop (Exec=$dwm_bin)"
    return 0
  fi

  sudo tee "$xsessions/dwm.desktop" >/dev/null <<EOF
[Desktop Entry]
Name=dwm
Comment=Suckless dynamic window manager
Exec=$dwm_bin
Type=Application
EOF
}

install_firacode() {
  run_cmd mkdir -p "$HOME/.local/share/fonts"
  local tmp
  tmp="$(mktemp -d)"
  run_sh "cd '$tmp' && curl -L -o Fira_Code.zip https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip && unzip -q Fira_Code.zip && cp -f ttf/*.ttf '$HOME/.local/share/fonts/'"
  run_cmd rm -rf "$tmp"
  run_cmd fc-cache -f
}

set_default_shell_zsh() {
  if [[ "${SHELL:-}" == "/bin/zsh" ]]; then
    return 0
  fi
  run_cmd chsh -s /bin/zsh "$USER" || true
}

install_floorp() {
  # Ensure gpg exists (you already install gnupg in base packages)
  if [[ "$DRY_RUN" -eq 1 ]]; then
    log "[DRY RUN] install floorp repo + key + apt install floorp"
    return 0
  fi

  run_cmd sudo install -d -m 0755 /usr/share/keyrings
  run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

  # Key
  run_cmd bash -lc "curl -fsSL https://ppa.floorp.app/KEY.gpg | sudo gpg --dearmor -o /usr/share/keyrings/Floorp.gpg"
  # Repo list
  run_cmd sudo curl -sS --compressed -o /etc/apt/sources.list.d/Floorp.list "https://ppa.floorp.app/Floorp.list"

  run_cmd sudo apt-get update -y
  run_cmd sudo apt-get install -y floorp
}

# ----------------------------
# UI selection
# ----------------------------
main_menu() {
  whiptail --title "Ubuntu dwm Installer" --checklist "Select what to install:" 20 90 10 \
    "packages" "Install required packages (X + startx + build deps)" ON \
    "stow" "Stow dotfiles into HOME" ON \
    "suckless" "Build+install dwm+dmenu+slstatus" ON \
    "session" "Register dwm in GDM login sessions" ON \
    "fonts" "Install Fira Code" ON \
    "floorp" "Install Floorp browser (ppa.floorp.app)" OFF \
    "shell" "Set default shell to zsh" ON \
    3>&1 1>&2 2>&3
}

# ----------------------------
# Main
# ----------------------------
main() {
  : >"$LOG_FILE"
  log "=== dotfiles installer start (dry_run=$DRY_RUN) ==="

  require_ubuntu
  ensure_sudo
  ensure_whiptail

  local selected
  selected="$(main_menu)" || exit 1

  start_gauge

  # Track what ran for final summary
  local ran=()
  local pct=0

  if grep -q "\"packages\"" <<<"$selected"; then
    pct=5; gauge $pct "Installing system packages…"
    install_base_packages
    ran+=("packages")
  fi

  if grep -q "\"stow\"" <<<"$selected"; then
    pct=25; gauge $pct "Stowing dotfiles…"
    stow_dotfiles
    ran+=("stow")
  fi

  if grep -q "\"suckless\"" <<<"$selected"; then
    pct=45; gauge $pct "Building and installing suckless tools…"
    build_install_suckless
    ran+=("suckless")
  fi

  if grep -q "\"session\"" <<<"$selected"; then
    pct=70; gauge $pct "Registering dwm session…"
    register_dwm_session
    ran+=("session")
  fi

  if grep -q "\"fonts\"" <<<"$selected"; then
    pct=80; gauge $pct "Installing fonts…"
    install_firacode
    ran+=("fonts")
  fi

  if grep -q "\"floorp\"" <<<"$selected"; then
    pct=88; gauge $pct "Installing Floorp…"
    install_floorp
    ran+=("floorp")
  fi

  if grep -q "\"shell\"" <<<"$selected"; then
    pct=96; gauge $pct "Setting default shell…"
    set_default_shell_zsh
    ran+=("shell")
  fi

  finish_gauge

  local summary="Completed."
  if [[ "$DRY_RUN" -eq 1 ]]; then
    summary="Dry run completed (no changes made)."
  fi

  local ran_text
  ran_text="$(printf "%s\n" "${ran[@]:-none}" | sed 's/^/- /')"

  whiptail --title "Installation Complete" --msgbox \
"$summary

Ran steps:
$ran_text

Next steps:
- Log out
- Click the session icon on the login screen
- Select 'dwm'
- Log in

Log file:
$LOG_FILE" 18 90

  if whiptail --title "View Log" --yesno "Open the install log now?" 10 60; then
    whiptail --title "Install Log" --textbox "$LOG_FILE" 28 110
  fi

  log "=== dotfiles installer end ==="
}

main "$@u