#!/usr/bin/env bash
set -Eeuo pipefail

DRY_RUN=0
if [[ "${1:-}" == "--dry-run" ]]; then DRY_RUN=1; fi

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"
mkdir -p "$LOG_DIR"
exec 3>&1

log() { printf "%s\n" "$*" >> "$LOG_FILE"; }

die_ui() {
    local msg="$1"
    if [[ -n "${GAUGE_FD_OPEN:-}" ]]; then exec 4>&- || true; GAUGE_FD_OPEN=""; fi
    whiptail --title "Error" --msgbox "$msg\n\nLog:\n$LOG_FILE" 16 90
    if whiptail --title "View Log" --yesno "Open the install log now?" 10 60; then
        whiptail --title "Install Log" --textbox "$LOG_FILE" 28 110
    fi
    exit 1
}

trap 'die_ui "Failed at line $LINENO:\n$BASH_COMMAND\n\nLast log lines:\n$(tail -n 120 "$LOG_FILE" 2>/dev/null || true)"' ERR

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
    whiptail --title "Privileges" --msgbox \
"sudo privileges are required.\n\nYou may be prompted for your password in the terminal.\n\nPress OK to continue." \
12 80
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

start_gauge() {
    exec 4> >(whiptail --title "Ubuntu dwm Installer" \
        --backtitle "Installing… (log: ~/.local/state/dotfiles-installer/install.log)" \
        --gauge "Preparing…" 12 90 0)
    GAUGE_FD_OPEN=1
}

gauge() { echo "$1" >&4; echo "# $2" >&4; }

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

run_shell() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] bash -lc $1"
        return 0
    fi
    bash -lc "$1" >>"$LOG_FILE" 2>&1
}

install_base_packages() {
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get upgrade -y

    run_cmd sudo apt-get install -y \
        git curl ca-certificates wget unzip fontconfig \
        stow \
        build-essential pkg-config gcc make \
        zsh tmux fzf tree ripgrep fd-find \
        xclip playerctl flameshot alacritty \
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

    local modules=(shell nvim tmux alacritty scripts wallpapers xinit-desktop)

    local tmp
    tmp="$(mktemp)"
    if ! (cd "$DOTFILES_DIR" && stow -n -v -t "$HOME" "${modules[@]}") >"$tmp" 2>&1; then
        whiptail --title "Stow Conflicts Detected" --msgbox \
"Stow reports conflicts (existing files in \$HOME).\n\nChoose how to proceed." 12 80
        whiptail --title "Stow Output" --textbox "$tmp" 28 110
        if whiptail --title "Resolve Conflicts" --yesno \
"SAFE: Abort and fix conflicts manually.\n\nRISKY: Use --adopt to pull existing files into the stow tree.\n\nUse --adopt?" 16 90; then
            rm -f "$tmp"
            (cd "$DOTFILES_DIR" && run_cmd stow --adopt -v -t "$HOME" "${modules[@]}")
        else
            rm -f "$tmp"
            die_ui "Aborted due to stow conflicts. Resolve conflicts and re-run."
        fi
    else
        rm -f "$tmp"
        (cd "$DOTFILES_DIR" && run_cmd stow -v -t "$HOME" "${modules[@]}")
    fi
}

ensure_suckless_submodules() {
    [[ -d "$DOTFILES_DIR/.git" ]] || die_ui "Dotfiles repo is not a git repository: $DOTFILES_DIR"
    [[ -f "$DOTFILES_DIR/.gitmodules" ]] || die_ui "Missing .gitmodules in $DOTFILES_DIR."
    run_shell "cd '$DOTFILES_DIR' && git submodule update --init --recursive"

    [[ -d "$DOTFILES_DIR/suckless/dwm" ]] || die_ui "Missing $DOTFILES_DIR/suckless/dwm"
    [[ -d "$DOTFILES_DIR/suckless/dmenu" ]] || die_ui "Missing $DOTFILES_DIR/suckless/dmenu"
    [[ -d "$DOTFILES_DIR/suckless/slstatus" ]] || die_ui "Missing $DOTFILES_DIR/suckless/slstatus"
}

link_suckless_into_code() {
    run_cmd mkdir -p "$HOME/code"
    for t in dwm dmenu slstatus; do
        if [[ -e "$HOME/code/$t" || -L "$HOME/code/$t" ]]; then
            run_cmd rm -rf "$HOME/code/$t"
        fi
    done

    run_cmd ln -s "$DOTFILES_DIR/suckless/dwm" "$HOME/code/dwm"
    run_cmd ln -s "$DOTFILES_DIR/suckless/dmenu" "$HOME/code/dmenu"
    run_cmd ln -s "$DOTFILES_DIR/suckless/slstatus" "$HOME/code/slstatus"
}

build_install_suckless() {
    for t in dwm dmenu slstatus; do
        [[ -d "$HOME/code/$t" ]] || die_ui "Missing $HOME/code/$t. Did linking fail?"
        run_shell "cd '$HOME/code/$t' && make clean && make"
        run_shell "cd '$HOME/code/$t' && sudo make install"
    done
}

register_dwm_session() {
    local xsessions="/usr/share/xsessions"
    [[ -d "$xsessions" ]] || { log "No $xsessions found; skipping session registration."; return 0; }

    local dwm_bin="/usr/local/bin/dwm"
    [[ -x "$dwm_bin" ]] || dwm_bin="$(command -v dwm || true)"
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
    run_shell "cd '$tmp' && curl -L -o Fira_Code.zip https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip && unzip -q Fira_Code.zip && cp -f ttf/*.ttf '$HOME/.local/share/fonts/'"
    run_cmd rm -rf "$tmp"
    run_cmd fc-cache -f
}

install_floorp() {
    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

    run_shell "curl -fsSL https://ppa.floorp.app/KEY.gpg | sudo gpg --dearmor -o /usr/share/keyrings/Floorp.gpg"
    run_cmd sudo curl -sS --compressed -o /etc/apt/sources.list.d/Floorp.list "https://ppa.floorp.app/Floorp.list"
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y floorp
}

set_default_shell_zsh() {
    [[ "${SHELL:-}" == "/bin/zsh" ]] && return 0
    run_cmd chsh -s /bin/zsh "$USER" || true
}

main_menu() {
    whiptail --title "Ubuntu dwm Installer" --checklist "Select what to install:" 22 92 12 \
        "packages" "Install required packages (X + startx + build deps)" ON \
        "stow" "Stow dotfiles into HOME" ON \
        "suckless" "Init submodules + symlink into ~/code + build+install" ON \
        "session" "Register dwm in GDM login sessions" ON \
        "fonts" "Install Fira Code" ON \
        "floorp" "Install Floorp browser (ppa.floorp.app)" ON \
        "shell" "Set default shell to zsh" ON \
        3>&1 1>&2 2>&3
}

main() {
    : >"$LOG_FILE"
    log "=== dotfiles installer start (dry_run=$DRY_RUN) ==="

    require_ubuntu
    ensure_sudo
    ensure_whiptail

    [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles directory not found: $DOTFILES_DIR"

    local selected
    selected="$(main_menu)" || exit 1

    start_gauge

    local ran=()
    local pct=0

    if grep -q "\"packages\"" <<<"$selected"; then
        pct=5; gauge $pct "Installing system packages…"
        install_base_packages
        ran+=("packages")
    fi

    if grep -q "\"stow\"" <<<"$selected"; then
        pct=20; gauge $pct "Stowing dotfiles…"
        stow_dotfiles
        ran+=("stow")
    fi

    if grep -q "\"suckless\"" <<<"$selected"; then
        pct=35; gauge $pct "Initializing suckless submodules…"
        ensure_suckless_submodules
        pct=45; gauge $pct "Linking suckless repos into ~/code…"
        link_suckless_into_code
        pct=60; gauge $pct "Building and installing dwm/dmenu/slstatus…"
        build_install_suckless
        ran+=("suckless")
    fi

    if grep -q "\"session\"" <<<"$selected"; then
        pct=80; gauge $pct "Registering dwm session…"
        register_dwm_session
        ran+=("session")
    fi

    if grep -q "\"fonts\"" <<<"$selected"; then
        pct=88; gauge $pct "Installing fonts…"
        install_firacode
        ran+=("fonts")
    fi

    if grep -q "\"floorp\"" <<<"$selected"; then
        pct=93; gauge $pct "Installing Floorp…"
        install_floorp
        ran+=("floorp")
    fi

    if grep -q "\"shell\"" <<<"$selected"; then
        pct=97; gauge $pct "Setting default shell…"
        set_default_shell_zsh
        ran+=("shell")
    fi

    finish_gauge

    local summary="Completed."
    [[ "$DRY_RUN" -eq 1 ]] && summary="Dry run completed (no changes made)."

    local ran_text
    ran_text="$(printf "%s\n" "${ran[@]:-none}" | sed 's/^/- /')"

    whiptail --title "Installation Complete" --msgbox \
"$summary

Ran steps:
$ran_text

Next steps:
- Log out
- Select 'dwm' session
- Log in

Log file:
$LOG_FILE" 18 90

    if whiptail --title "View Log" --yesno "Open the install log now?" 10 60; then
        whiptail --title "Install Log" --textbox "$LOG_FILE" 28 110
    fi

    log "=== dotfiles installer end ==="
}

main "$@"