#!/usr/bin/env bash
set -Eeuo pipefail

export NEWT_COLORS='
root=,black
window=white,black
border=brightblack,black
title=brightcyan,black
textbox=white,black
button=black,brightcyan
actbutton=black,cyan
compactbutton=white,black
listbox=white,black
actlistbox=black,brightcyan
actsellistbox=black,cyan
checkbox=white,black
actcheckbox=black,brightcyan
entry=white,black
label=white,black
helpline=brightblack,black
roottext=white,black
emptyscale=brightblack,black
fullscale=cyan,black
disentry=brightblack,black
'

DRY_RUN=0
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=1
fi

DOTFILES_DIR="${DOTFILES_DIR:-$HOME/.dotfiles}"
REPOS_DIR="${REPOS_DIR:-$HOME/repos}"

VWM_REPO="${VWM_REPO:-git@github.com:veasman/vwm.git}"
LOOM_REPO="${LOOM_REPO:-git@github.com:veasman/loom.git}"
PMUX_REPO="${PMUX_REPO:-git@github.com:veasman/pmux.git}"

VWM_DIR="$REPOS_DIR/vwm"
LOOM_DIR="$REPOS_DIR/loom"
PMUX_DIR="$REPOS_DIR/pmux"
YAY_DIR="$REPOS_DIR/yay"

LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"
mkdir -p "$LOG_DIR"

SUDO_KEEPALIVE_PID=""
GAUGE_OPEN=""
CURRENT_PCT=0
CURRENT_MSG="Preparing..."
GAUGE_TICK=0

INSTALL_SUNSHINE=0
INSTALL_STEAM=0
INSTALL_MULLVAD=0

WARNINGS=()

log() {
    printf "%s\n" "$*" >> "$LOG_FILE"
}

warn() {
    WARNINGS+=("$1")
    log "[warn] $1"
}

die_ui() {
    local msg="$1"

    if [[ -n "${GAUGE_OPEN:-}" ]]; then
        exec 4>&- || true
        GAUGE_OPEN=""
    fi

    whiptail --title "Error" --msgbox "$msg\n\nLog:\n$LOG_FILE" 18 100

    if whiptail --title "View Log" --yesno "Open install log now?" 10 60; then
        whiptail --title "Install Log" --textbox "$LOG_FILE" 28 120
    fi

    exit 1
}

require_arch() {
    [[ -r /etc/os-release ]] || die_ui "/etc/os-release not found"

    # shellcheck disable=SC1091
    . /etc/os-release

    if [[ "${ID:-}" != "arch" && "${ID_LIKE:-}" != *"arch"* ]]; then
        die_ui "This installer is for Arch Linux."
    fi
}

ensure_sudo() {
    command -v sudo >/dev/null 2>&1 || die_ui "sudo not found"

    whiptail --title "Privileges" --msgbox \
"sudo is required.\n\nYou may be prompted in the terminal." 10 70

    sudo -v >>"$LOG_FILE" 2>&1
    ( while true; do sudo -n true; sleep 60; done ) 2>/dev/null &
    SUDO_KEEPALIVE_PID=$!
    trap 'kill "${SUDO_KEEPALIVE_PID:-0}" 2>/dev/null || true' EXIT
}

ensure_whiptail() {
    if command -v whiptail >/dev/null 2>&1; then
        return
    fi

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] sudo pacman -Sy --noconfirm libnewt"
        return 0
    fi

    sudo pacman -Sy --noconfirm libnewt >>"$LOG_FILE" 2>&1
}

start_gauge() {
    exec 4> >(whiptail --title "Arch dotfiles bootstrap" \
        --backtitle "Installing...  Log: $LOG_FILE" \
        --gauge "Preparing..." 10 80 0)
    GAUGE_OPEN=1
}

gauge_write() {
    local pct="$1"
    local msg="$2"
    [[ -n "${GAUGE_OPEN:-}" ]] || return 0

    {
        echo "XXX"
        echo "$pct"
        printf "%s\n" "$msg"
        echo "XXX"
    } >&4
}

step() {
    local pct="$1"
    local msg="$2"
    CURRENT_PCT="$pct"
    CURRENT_MSG="$msg"
    GAUGE_TICK=0
    gauge_write "$CURRENT_PCT" "$CURRENT_MSG"
}

finish_gauge() {
    step 100 "Finalizing..."
    sleep 1
    exec 4>&-
    GAUGE_OPEN=""
}

gauge_pump_while_pid() {
    local pid="$1"
    local spinner

    while kill -0 "$pid" 2>/dev/null; do
        GAUGE_TICK=$((GAUGE_TICK + 1))

        case $((GAUGE_TICK % 4)) in
            0) spinner="|" ;;
            1) spinner="/" ;;
            2) spinner="-" ;;
            3) spinner="\\" ;;
        esac

        gauge_write "$CURRENT_PCT" "$CURRENT_MSG [$spinner]"
        sleep 0.5
    done
}

run_cmd() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] $*"
        return 0
    fi

    log "[cmd] $*"
    "$@" >>"$LOG_FILE" 2>&1 &
    local pid=$!

    if [[ -n "${GAUGE_OPEN:-}" ]]; then
        gauge_pump_while_pid "$pid"
    fi

    set +e
    wait "$pid"
    local rc=$?
    set -e

    if [[ "$rc" -ne 0 ]]; then
        die_ui "Command failed (exit=$rc).\n\nCommand:\n$*\n\nLast log lines:\n$(tail -n 40 "$LOG_FILE" 2>/dev/null || true)"
    fi
}

run_shell() {
    local cmd="$1"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] bash -lc $cmd"
        return 0
    fi

    log "[shell] bash -lc $cmd"
    bash -lc "set -eo pipefail; $cmd" >>"$LOG_FILE" 2>&1 &
    local pid=$!

    if [[ -n "${GAUGE_OPEN:-}" ]]; then
        gauge_pump_while_pid "$pid"
    fi

    set +e
    wait "$pid"
    local rc=$?
    set -e

    if [[ "$rc" -ne 0 ]]; then
        die_ui "Shell command failed (exit=$rc).\n\nCommand:\n$cmd\n\nLast log lines:\n$(tail -n 40 "$LOG_FILE" 2>/dev/null || true)"
    fi
}

optional_prompt() {
    local result=""
    result="$(
        whiptail --title "Optional installs" --checklist \
        "Choose optional software:" 16 90 6 \
        "sunshine" "Install Sunshine (AUR)" OFF \
        "steam" "Install Steam" OFF \
        "mullvad" "Install Mullvad VPN (AUR)" OFF \
        3>&1 1>&2 2>&3
    )" || die_ui "Aborted"

    INSTALL_SUNSHINE=0
    INSTALL_STEAM=0
    INSTALL_MULLVAD=0

    [[ "$result" == *'"sunshine"'* ]] && INSTALL_SUNSHINE=1
    [[ "$result" == *'"steam"'* ]] && INSTALL_STEAM=1
    [[ "$result" == *'"mullvad"'* ]] && INSTALL_MULLVAD=1

    log "[optional] sunshine=$INSTALL_SUNSHINE steam=$INSTALL_STEAM mullvad=$INSTALL_MULLVAD"
}

ensure_dirs() {
    run_cmd mkdir -p \
        "$HOME/.config" \
        "$HOME/.local/bin" \
        "$HOME/.local/share" \
        "$HOME/.local/state" \
        "$REPOS_DIR"
}

pacman_install() {
    run_cmd sudo pacman -S --needed --noconfirm "$@"
}

enable_multilib_if_needed() {
    if grep -Eq '^\[multilib\]' /etc/pacman.conf && grep -Eq '^Include = /etc/pacman.d/mirrorlist' /etc/pacman.conf; then
        return 0
    fi

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] enable multilib in /etc/pacman.conf"
        return 0
    fi

    run_shell '
        sudo cp /etc/pacman.conf /etc/pacman.conf.bak-dotfiles
        sudo sed -i "/^\#\[multilib\]/,/^\#Include = \/etc\/pacman.d\/mirrorlist/ s/^#//" /etc/pacman.conf
    '
    run_cmd sudo pacman -Sy
}

install_cpu_microcode() {
    if grep -qi "AuthenticAMD" /proc/cpuinfo 2>/dev/null; then
        log "[cpu] detected AMD CPU"
        pacman_install amd-ucode
    elif grep -qi "GenuineIntel" /proc/cpuinfo 2>/dev/null; then
        log "[cpu] detected Intel CPU"
        pacman_install intel-ucode
    else
        warn "Could not determine CPU vendor for microcode install; skipping"
    fi
}

install_base_packages() {
    step 5 "Refreshing pacman metadata"
    run_cmd sudo pacman -Sy

    step 10 "Installing bootstrap prerequisites"
    pacman_install base-devel git stow curl wget unzip python python-pip pkgconf libnewt

    step 18 "Installing shell and terminal tools"
    pacman_install zsh tmux fzf tree ripgrep fd xclip

    step 26 "Installing X11 desktop utilities"
    pacman_install \
        xorg-server xorg-xinit xorg-xrandr xorg-xsetroot \
        rofi dunst libnotify picom xwallpaper playerctl flameshot brightnessctl

    step 34 "Installing VWM build dependencies"
    pacman_install \
        libx11 libxinerama libxft libxrender cairo \
        libxcb xcb-util xcb-util-randr xcb-util-keysyms xcb-util-wm fontconfig

    step 42 "Installing system utilities"
    pacman_install \
        linux-firmware pciutils usbutils lm_sensors mesa vulkan-tools vulkan-icd-loader

    step 50 "Installing LaTeX tooling"
    pacman_install texlive-basic texlive-latexextra latexmk zathura zathura-pdf-poppler

    step 56 "Installing Node and npm"
    pacman_install nodejs-lts-iron npm

    step 60 "Installing treesitter-cli"
    run_cmd sudo npm install -g treesitter-cli

    install_cpu_microcode
}

ensure_yay() {
    if command -v yay >/dev/null 2>&1; then
        return 0
    fi

    clone_or_update_repo "https://aur.archlinux.org/yay.git" "$YAY_DIR"
    run_shell "cd '$YAY_DIR' && makepkg -si --noconfirm"
}

yay_install() {
    ensure_yay
    run_shell "yay -S --needed --noconfirm $*"
}

install_floorp() {
    yay_install floorp-bin
}

install_tailscale() {
    pacman_install tailscale
    run_cmd sudo systemctl enable --now tailscaled
}

tailscale_post() {
    if tailscale status >/dev/null 2>&1; then
        return 0
    fi

    warn "Tailscale is installed but not authenticated yet"

    if whiptail --title "Tailscale" --yesno \
"Run 'sudo tailscale up' now?\n\nThis will begin login/auth." 12 72; then
        if [[ "$DRY_RUN" -eq 0 ]]; then
            sudo tailscale up || warn "tailscale up did not complete successfully"
        fi
    else
        warn "Skipped 'tailscale up'. Run it manually later."
    fi
}

install_mullvad() {
    yay_install mullvad-vpn-bin
}

install_docker() {
    pacman_install docker docker-buildx docker-compose
    run_cmd sudo systemctl enable --now docker
    run_cmd sudo usermod -aG docker "$USER"
}

install_neovim_nightly() {
    yay_install neovim-nightly-bin
}

clone_or_update_repo() {
    local repo_url="$1"
    local target_dir="$2"

    if [[ -d "$target_dir/.git" ]]; then
        run_shell "cd '$target_dir' && git checkout master && git fetch && git pull origin master"
        return 0
    fi

    if [[ -e "$target_dir" && ! -d "$target_dir/.git" ]]; then
        die_ui "Target exists but is not a git repo: $target_dir"
    fi

    run_shell "git clone '$repo_url' '$target_dir'"
}

install_vwm() {
    clone_or_update_repo "$VWM_REPO" "$VWM_DIR"
    run_shell "cd '$VWM_DIR' && make && sudo make install"
}

install_loom() {
    clone_or_update_repo "$LOOM_REPO" "$LOOM_DIR"
    run_shell "cd '$LOOM_DIR' && make install"
}

install_pmux() {
    clone_or_update_repo "$PMUX_REPO" "$PMUX_DIR"
    run_shell "cd '$PMUX_DIR' && make install"
}

delete_stow_conflicts_for_package() {
    local package_dir="$1"
    local target_root="$2"

    while IFS= read -r -d '' src; do
        local rel="${src#"$package_dir"/}"
        [[ -z "$rel" ]] && continue

        local dst="$target_root/$rel"

        if [[ -L "$dst" ]]; then
            continue
        fi

        if [[ -e "$dst" ]]; then
            log "[stow] removing conflict: $dst"
            run_cmd rm -rf "$dst"
        fi
    done < <(find "$package_dir" -mindepth 1 -print0)
}

stow_package_force() {
    local package_name="$1"
    local package_dir="$DOTFILES_DIR/$package_name"

    [[ -d "$package_dir" ]] || {
        warn "Missing stow package: $package_name"
        return 0
    }

    delete_stow_conflicts_for_package "$package_dir" "$HOME"
    run_shell "cd '$DOTFILES_DIR' && stow -v -R -t '$HOME' '$package_name'"
}

stow_dotfiles() {
    local packages=(
        floorp
        fonts
        git
        kitty
        latex
        nvim
        pmux
        rofi
        scripts
        shell
        tmux
        vwm
        xprofile-desktop
        xresources
    )

    for pkg in "${packages[@]}"; do
        stow_package_force "$pkg"
    done

    run_cmd fc-cache -f
}

generate_xinitrc() {
    local xinitrc="$HOME/.xinitrc"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] write $xinitrc"
        return 0
    fi

    cat > "$xinitrc" <<'EOF'
#!/usr/bin/env sh

[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

if [ -x /usr/bin/dbus-update-activation-environment ]; then
    dbus-update-activation-environment --systemd DISPLAY XAUTHORITY
fi

exec vwm
EOF

    chmod +x "$xinitrc"
}

ensure_zsh_default_shell() {
    local user="${SUDO_USER:-$USER}"
    local target_shell
    target_shell="$(command -v zsh || true)"

    [[ -n "$target_shell" ]] || die_ui "zsh not found"

    if ! grep -qx "$target_shell" /etc/shells; then
        echo "$target_shell" | sudo tee -a /etc/shells >/dev/null
    fi

    local current_shell
    current_shell="$(getent passwd "$user" | cut -d: -f7)"

    if [[ "$current_shell" != "$target_shell" ]]; then
        run_cmd sudo usermod -s "$target_shell" "$user"
    fi
}

apply_default_theme() {
    if ! command -v loom >/dev/null 2>&1; then
        warn "loom not found in PATH; skipping default theme apply"
        return 0
    fi

    run_shell 'loom apply gruvbox'
}

install_steam() {
    enable_multilib_if_needed
    pacman_install steam
}

install_sunshine() {
    yay_install sunshine-bin
}

tampermonkey_reminder() {
    local zip="$DOTFILES_DIR/assets/tampermonkey/tampermonkey-backup.zip"
    [[ -f "$zip" ]] || return 0

    whiptail --title "Tampermonkey reminder" --msgbox \
"Tampermonkey backup detected:\n$zip\n\nImport it manually inside Floorp:\n1) Open Floorp\n2) Open Tampermonkey dashboard\n3) Utilities -> Import\n4) Select the zip file" 18 100
}

post_install_notes() {
    local warns="none"
    if [[ "${#WARNINGS[@]}" -gt 0 ]]; then
        warns="$(printf '%s\n' "${WARNINGS[@]}" | sed 's/^/- /')"
    fi

    local notes="Completed.

Warnings:
$warns

Next steps:
- Run: startx
- If Tailscale is not connected yet, run: sudo tailscale up
- Run: docker run hello-world after re-login to confirm Docker group access
- Open Floorp and manually import the Tampermonkey backup if you want it
- Reboot if graphics/input acts stupid
"

    if [[ "$INSTALL_SUNSHINE" -eq 1 ]]; then
        notes+="
Sunshine:
- Launch Sunshine
- Complete host setup and pairing
"
    fi

    if [[ "$INSTALL_MULLVAD" -eq 1 ]]; then
        notes+="
Mullvad:
- Launch Mullvad VPN
- Log in with your account number
"
    fi

    whiptail --title "Install complete" --msgbox "$notes\nLog:\n$LOG_FILE" 28 110
}

main() {
    : >"$LOG_FILE"
    log "=== arch bootstrap start (dry_run=$DRY_RUN) ==="

    require_arch
    ensure_sudo
    ensure_whiptail
    ensure_dirs
    optional_prompt

    [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles directory not found: $DOTFILES_DIR"

    start_gauge

    step 5 "Installing Arch packages"
    install_base_packages

    step 20 "Installing Floorp"
    install_floorp

    step 28 "Installing Tailscale"
    install_tailscale

    step 36 "Installing Docker"
    install_docker

    step 44 "Installing Neovim nightly"
    install_neovim_nightly

    step 56 "Installing vwm repo"
    install_vwm

    step 66 "Installing loom repo"
    install_loom

    step 74 "Installing pmux repo"
    install_pmux

    step 84 "Stowing dotfiles with forced replacement"
    stow_dotfiles

    step 90 "Generating ~/.xinitrc for startx"
    generate_xinitrc

    step 95 "Setting default shell to zsh"
    ensure_zsh_default_shell

    if [[ "$INSTALL_SUNSHINE" -eq 1 ]]; then
        step 97 "Installing Sunshine"
        install_sunshine
    fi

    if [[ "$INSTALL_STEAM" -eq 1 ]]; then
        step 98 "Installing Steam"
        install_steam
    fi

    if [[ "$INSTALL_MULLVAD" -eq 1 ]]; then
        step 99 "Installing Mullvad"
        install_mullvad
    fi

    finish_gauge

    tailscale_post
    tampermonkey_reminder
    post_install_notes

    log "=== arch bootstrap end ==="
}

main "$@"
