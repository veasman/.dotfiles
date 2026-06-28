#!/usr/bin/env bash
set -Eeuo pipefail
export GIT_TERMINAL_PROMPT=0

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

PMUX_REPO="${PMUX_REPO:-git@github.com:veasman/pmux.git}"
HERMES_REPO="${HERMES_REPO:-https://github.com/NousResearch/hermes-agent.git}"

# TODO: loom-rs is not yet packaged — uncomment when ready
# LOOM_REPO="${LOOM_REPO:-git@github.com:veasman/loom.git}"
# LOOM_DIR="$REPOS_DIR/loom"

PMUX_DIR="$REPOS_DIR/pmux"
PARU_DIR="$REPOS_DIR/paru"
HERMES_DIR="$HOME/.hermes/hermes-agent"

LOG_DIR="/tmp/dotfiles-install"
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

WHIPTAIL_OK=0

# ---------------------------------------------------------------------------
# Init system detection
# ---------------------------------------------------------------------------
INIT_SYSTEM="unknown"
if [[ "$(readlink /sbin/init 2>/dev/null || true)" == *"systemd"* ]] || grep -q systemd /proc/1/comm 2>/dev/null; then
    INIT_SYSTEM="systemd"
elif command -v rc-update &>/dev/null; then
    INIT_SYSTEM="openrc"
fi

# ---------------------------------------------------------------------------
# Logging / UI helpers
# ---------------------------------------------------------------------------

log() {
    printf "%s\n" "$*" >> "$LOG_FILE"
}

warn() {
    WARNINGS+=("$1")
    log "[warn] $1"
}

die_ui() {
    local msg="$1"

    # Always print to stderr FIRST, before any whiptail attempt
    echo "[FATAL] $msg" >&2
    echo "  Log: $LOG_FILE" >&2

    if [[ -n "${GAUGE_OPEN:-}" ]]; then
        exec 4>&- || true
        GAUGE_OPEN=""
    fi

    if ! whiptail_run --title "Error" --msgbox "$msg\\n\\nLog:\\n$LOG_FILE" 18 100; then
        exit 1
    fi

    if whiptail_run --title "View Log" --yesno "Open install log now?" 10 60; then
        whiptail_run --title "Install Log" --textbox "$LOG_FILE" 28 120
    fi

    exit 1
}

# Safe whiptail wrapper: falls back to text prompts when running
# in a non-graphical session (SSH/tty) where whiptail can't open a terminal.
# Returns 0 on OK/Yes, non-zero on Cancel/No, matching whiptail's convention.
whiptail_run() {
    # args are forwarded verbatim to whiptail
    if [[ "$WHIPTAIL_OK" -eq 1 ]]; then
        whiptail "$@" 2>/dev/null </dev/tty
        return $?
    fi
    return 1
}

detect_terminal_ui() {
    # Test whether whiptail can render in this terminal.
    # Must come after ensure_whiptail so libnewt is installed.
    if [[ "$DRY_RUN" -eq 1 ]]; then
        return 0
    fi

    # Non-visual check: is stdout a real terminal with enough size?
    local rows cols
    rows=$(tput lines 2>/dev/null || echo 0)
    cols=$(tput cols 2>/dev/null || echo 0)
    if [[ -t 1 ]] && (( rows >= 10 && cols >= 40 )); then
        WHIPTAIL_OK=1
        log "[ui] whiptail available (terminal ${rows}x${cols})"
    else
        WHIPTAIL_OK=0
        log "[ui] whiptail unavailable (${rows}x${cols})"
    fi
}

# ---------------------------------------------------------------------------
# Prerequisite checks
# ---------------------------------------------------------------------------

check_prerequisites() {
    [[ "$(id -u)" -ne 0 ]] || die_ui "Do not run this script as root. Run as your regular user."

    [[ -d "$DOTFILES_DIR/.git" ]] || die_ui "Dotfiles not found at $DOTFILES_DIR\nRun pre-install.sh first."

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] skip network and SSH checks"
        return 0
    fi

    if ! ping -c1 -W3 archlinux.org &>/dev/null; then
        die_ui "No network connectivity. Check NetworkManager."
    fi

    local _ssh_out
    # ssh -T to github.com always exits 1 (shell access denied) — ignore exit,
    # check the actual output to confirm authentication.
    _ssh_out=$(ssh -o BatchMode=yes -o StrictHostKeyChecking=accept-new -T git@github.com 2>&1) || true
    if ! echo "$_ssh_out" | grep -qi "success\\|authenticated"; then
        die_ui "GitHub SSH authentication failed.\\nOutput: $_ssh_out"
    fi
}

require_arch() {
    [[ -r /etc/os-release ]] || die_ui "/etc/os-release not found"

    # shellcheck disable=SC1091
    . /etc/os-release

    if [[ "${ID:-}" != "artix" && "${ID:-}" != "arch" && "${ID_LIKE:-}" != *"arch"* ]]; then
        die_ui "This installer is for Arch/Artix Linux."
    fi
}

ensure_sudo() {
    command -v sudo >/dev/null 2>&1 || die_ui "sudo not found"
    # Credentials are cached and keepalive is running from check_sudo_early()
    if ! sudo -n true 2>/dev/null; then
        die_ui "sudo credentials expired despite keepalive. Try running: sudo -v"
    fi
}

setup_git_config() {
    git config --global user.name "veasman"
    git config --global user.email "charlton.moren@gmail.com"
    log "[git] configured user.name=veasman, user.email=charlton.moren@gmail.com"
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

# ---------------------------------------------------------------------------
# Gauge (progress bar) helpers
# ---------------------------------------------------------------------------

start_gauge() {
    if [[ "$WHIPTAIL_OK" -ne 1 ]]; then
        return 0
    fi
    exec 4> >(whiptail --title "Dotfiles bootstrap" \
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

# ---------------------------------------------------------------------------
# Command runners
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# Interactive prompts
# ---------------------------------------------------------------------------

optional_prompt() {
    local result=""
    local items=(sunshine steam mullvad gaming)

    if [[ "$WHIPTAIL_OK" -eq 1 ]]; then
        result="$(
            whiptail --title "Optional installs" --checklist \
            "Choose optional software:" 18 90 7 \
            "sunshine" "Install Sunshine (AUR)" OFF \
            "steam" "Install Steam" OFF \
            "mullvad" "Install Mullvad VPN (AUR)" OFF \
            "gaming" "Gaming tweaks + gamescope/mangohud/gamemode" OFF \
            3>&1 1>&2 2>&3
        )" || die_ui "Aborted"
    else
        echo "" >&2
        echo "Optional software (enter y/n for each):" >&2
        for item in "${items[@]}"; do
            local desc=""
            case "$item" in
                sunshine) desc="Install Sunshine (AUR)" ;;
                steam)    desc="Install Steam" ;;
                mullvad)  desc="Install Mullvad VPN (AUR)" ;;
                gaming)   desc="Gaming tweaks + gamescope/mangohud/gamemode" ;;
            esac
            while :; do
                local yn=""
                read -r -p "  $desc [y/N]: " yn </dev/tty
                case "$yn" in
                    [yY]*) result="$result \"$item\""; break ;;
                    [nN]*|"") break ;;
                esac
            done
        done
    fi

    INSTALL_SUNSHINE=0
    INSTALL_STEAM=0
    INSTALL_MULLVAD=0
    INSTALL_GAMING=0

    [[ "$result" == *'"sunshine"'* ]] && INSTALL_SUNSHINE=1
    [[ "$result" == *'"steam"'* ]] && INSTALL_STEAM=1
    [[ "$result" == *'"mullvad"'* ]] && INSTALL_MULLVAD=1
    [[ "$result" == *'"gaming"'* ]] && INSTALL_GAMING=1

    log "[optional] sunshine=$INSTALL_SUNSHINE steam=$INSTALL_STEAM mullvad=$INSTALL_MULLVAD gaming=$INSTALL_GAMING"
}

# ---------------------------------------------------------------------------
# Directory setup
# ---------------------------------------------------------------------------

ensure_dirs() {
    run_cmd mkdir -p \
        "$HOME/.config" \
        "$HOME/.local/bin" \
        "$HOME/.local/share" \
        "$HOME/.local/state" \
        "$REPOS_DIR"
}

# ---------------------------------------------------------------------------
# Package managers
# ---------------------------------------------------------------------------

pacman_install() {
    run_cmd sudo pacman -S --needed --noconfirm "$@"
}

ensure_paru() {
    if command -v paru >/dev/null 2>&1; then
        return 0
    fi

    clone_or_update_repo "https://aur.archlinux.org/paru.git" "$PARU_DIR"
    run_shell "rustup default stable"
    run_shell "cd '$PARU_DIR' && makepkg -s --noconfirm"
    # Install with explicit sudo (via keepalive) to avoid password prompt
    # from makepkg -i's internal sudo call.
    run_cmd sudo pacman -U --noconfirm "$PARU_DIR"/*.pkg.tar.zst
}

paru_install() {
    ensure_paru
    run_shell "paru -S --needed --noconfirm $*"
}

# ---------------------------------------------------------------------------
# Service helpers (init-agnostic)
# ---------------------------------------------------------------------------

svc_enable_start() {
    local svc="$1"
    case "$INIT_SYSTEM" in
        systemd)
            run_cmd sudo systemctl enable "$svc"
            run_cmd sudo systemctl start "$svc"
            ;;
        openrc)
            run_cmd sudo rc-update add "$svc" default
            run_cmd sudo rc-service "$svc" start
            ;;
    esac
}

svc_install_init_scripts() {
    # Copy OpenRC or systemd service files into place.
    # Called after stow_dotfiles so source files are guaranteed present.
    case "$INIT_SYSTEM" in
        openrc)
            local src="$DOTFILES_DIR/openrc"
            if [[ -d "$src" ]]; then
                for init in "$src"/*; do
                    [[ -f "$init" ]] || continue
                    local name; name="$(basename "$init")"
                    run_cmd sudo cp "$init" "/etc/init.d/$name"
                    run_cmd sudo chmod 755 "/etc/init.d/$name"
                done
            fi
            # Write /etc/conf.d/ overrides so init scripts use the current
            # user instead of the hardcoded "oracle" default.
            run_cmd sudo mkdir -p /etc/conf.d
            if [[ ! -f /etc/conf.d/freellmapi ]]; then
                run_cmd sudo sh -c "echo 'FREEMAPI_USER=$USER' > /etc/conf.d/freellmapi"
            fi
            if [[ ! -f /etc/conf.d/hermes-gateway ]]; then
                run_cmd sudo sh -c "echo 'HERMES_USER=$USER' > /etc/conf.d/hermes-gateway"
            fi
            if [[ ! -f /etc/conf.d/ollama ]]; then
                run_cmd sudo sh -c "echo 'OLLAMA_USER=$USER' > /etc/conf.d/ollama"
            fi
            ;;
        systemd)
            local src="$DOTFILES_DIR/systemd"
            if [[ -d "$src" ]]; then
                for unit in "$src"/*.service "$src"/*.timer; do
                    [[ -f "$unit" ]] || continue
                    run_cmd sudo cp "$unit" "/etc/systemd/system/"
                done
                run_cmd sudo systemctl daemon-reload
            fi
            ;;
    esac
}

# ---------------------------------------------------------------------------
# Package installation
# ---------------------------------------------------------------------------

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
        sudo sed -i "/^#\[multilib\]/s/^#//" /etc/pacman.conf
        sudo sed -i "/^#Include = \/etc\/pacman.d\/mirrorlist/s/^#//" /etc/pacman.conf
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
    step 3 "Refreshing pacman metadata"
    run_cmd sudo pacman -Sy

    step 5 "Installing bootstrap prerequisites"
    pacman_install base-devel git stow curl wget unzip pkgconf libnewt openssh

    step 8 "Installing shell and terminal tools"
    pacman_install zsh tmux fzf tree ripgrep fd jq htop glances \
                   lf imv chafa pulsemixer

    step 12 "Installing Hyprland desktop stack"
    pacman_install \
        hyprland hyprpaper hypridle hyprlock hyprshot hyprpicker \
        waybar fuzzel mako swaync libnotify \
        grim slurp wl-clipboard socat playerctl brightnessctl \
        xdg-utils xdg-desktop-portal xdg-desktop-portal-gtk \
        xdg-desktop-portal-hyprland

    step 16 "Installing audio stack"
    pacman_install \
        pipewire pipewire-pulse \
        wireplumber pavucontrol

    step 19 "Installing init service scripts"
    if [[ "$INIT_SYSTEM" == "openrc" ]]; then
        pacman_install \
            pipewire-openrc pipewire-pulse-openrc wireplumber-openrc
    fi

    step 22 "Installing bluetooth"
    if [[ "$INIT_SYSTEM" == "openrc" ]]; then
        pacman_install bluez bluez-utils bluez-openrc
    else
        pacman_install bluez bluez-utils
    fi

    step 30 "Installing system utilities"
    pacman_install \
        linux-firmware pciutils usbutils lm_sensors \
        mesa vulkan-tools vulkan-icd-loader \
        mpv pcmanfm

    step 33 "Installing development tools"
    pacman_install python python-pip python-pillow go rustup nvm uv


    step 39 "Installing Node LTS via nvm"
    run_shell "source /usr/share/nvm/init-nvm.sh && nvm install --lts"

    step 41 "Installing tree-sitter-cli"
    pacman_install tree-sitter-cli

    install_cpu_microcode
}

# ---------------------------------------------------------------------------
# Third-party / AUR packages
# ---------------------------------------------------------------------------

install_floorp() {
    paru_install floorp-bin
}

install_tailscale() {
    if [[ "$INIT_SYSTEM" == "openrc" ]]; then
        pacman_install tailscale tailscale-openrc
    else
        pacman_install tailscale
    fi
    svc_enable_start tailscaled
}

install_docker() {
    if [[ "$INIT_SYSTEM" == "openrc" ]]; then
        pacman_install docker docker-compose docker-openrc
    else
        pacman_install docker docker-compose
    fi
    svc_enable_start docker
    run_cmd sudo usermod -aG docker "$USER"
}

install_neovim_nightly() {
    paru_install neovim-git
}

install_mullvad() {
    paru_install mullvad-vpn-bin
}

install_steam() {
    enable_multilib_if_needed
    pacman_install steam
}

install_gaming_tweaks() {
    enable_multilib_if_needed
    pacman_install gamescope mangohud gamemode lib32-gamemode
    paru_install proton-ge-custom-bin 2>/dev/null || true

    # Deploy sysctl tweaks
    local sysctl_src="$DOTFILES_DIR/gaming/etc/sysctl.d/99-gaming.conf"
    if [[ -f "$sysctl_src" ]]; then
        run_cmd sudo cp "$sysctl_src" /etc/sysctl.d/99-gaming.conf
        run_cmd sudo sysctl --system
    fi

    # Deploy udev rules
    local udev_src="$DOTFILES_DIR/gaming/etc/udev/rules.d/99-gaming-input.rules"
    if [[ -f "$udev_src" ]]; then
        run_cmd sudo cp "$udev_src" /etc/udev/rules.d/99-gaming-input.rules
        run_cmd sudo udevadm control --reload-rules
        run_cmd sudo udevadm trigger
    fi

    # Deploy modprobe config
    local mod_src="$DOTFILES_DIR/gaming/etc/modprobe.d/99-gaming.conf"
    if [[ -f "$mod_src" ]]; then
        run_cmd sudo cp "$mod_src" /etc/modprobe.d/99-gaming.conf
    fi
}

install_sunshine() {
    paru_install sunshine-bin
}

install_swayosd() {
    # AUR. Tiny GTK HUD daemon used by the volume / brightness wrappers
    # for centered overlay popups when on Hyprland.
    paru_install swayosd-git
}

install_termfilechooser() {
    # Routes GTK file dialogs (Floorp uploads, etc.) to lf in
    # kara-toe-client via xdg-desktop-portal-termfilechooser. Config
    # lives in ~/.config/xdg-desktop-portal-termfilechooser/config and
    # ~/.config/xdg-desktop-portal/portals.conf (both stowed via the
    # gtk package).
    paru_install xdg-desktop-portal-termfilechooser-hunkyburrito-git
}

install_hyprland_share_picker_preview() {
    # Replaces the bundled hyprland-share-picker (text-only) with a
    # Qt UI that shows visual thumbnails of every window/monitor —
    # so MS Teams and similar prompt you with what you'd actually
    # share rather than a raw output-name list.
    paru_install hyprland-share-picker-preview-git
}

# ---------------------------------------------------------------------------
# Custom projects (clone + make install)
# ---------------------------------------------------------------------------

clone_or_update_repo() {
    local repo_url="$1"
    local target_dir="$2"

    if [[ -d "$target_dir/.git" ]]; then
        run_shell "cd '$target_dir' && git fetch && git checkout -B local-sync origin/HEAD && git merge --ff-only"
        return 0
    fi

    if [[ -e "$target_dir" && ! -d "$target_dir/.git" ]]; then
        die_ui "Target exists but is not a git repo: $target_dir"
    fi

    run_shell "git clone '$repo_url' '$target_dir'"
}

install_pmux() {
    clone_or_update_repo "$PMUX_REPO" "$PMUX_DIR"
    run_shell "cd '$PMUX_DIR' && make install"
}

install_hermes() {
    HERMES_BASE="$(dirname "$HERMES_DIR")"
    run_cmd mkdir -p "$HERMES_BASE"

    clone_or_update_repo "$HERMES_REPO" "$HERMES_DIR"

    if [[ ! -d "$HERMES_DIR/venv" ]]; then
        run_shell "cd '$HERMES_DIR' && uv venv --python 3.13 venv && uv pip install -e ."
    fi

    local cfg_dst="$HOME/.hermes/config.yaml"
    local cfg_src="$DOTFILES_DIR/hermes/config.yaml.example"
    if [[ ! -f "$cfg_dst" ]] && [[ -f "$cfg_src" ]]; then
        run_cmd cp "$cfg_src" "$cfg_dst"
    fi

    local env_dst="$HOME/.hermes/.env"
    local env_src="$DOTFILES_DIR/hermes/.env.template"
    if [[ ! -f "$env_dst" ]] && [[ -f "$env_src" ]]; then
        run_cmd cp "$env_src" "$env_dst"
        run_cmd chmod 600 "$env_dst"
    fi
}

install_freellmapi() {
    FREEMAPI_REPO="${FREEMAPI_REPO:-https://github.com/tashfeenahmed/freellmapi.git}"
    FREEMAPI_DIR="$HOME/repos/freellmapi"

    clone_or_update_repo "$FREEMAPI_REPO" "$FREEMAPI_DIR"

    local env_src="$FREEMAPI_DIR/.env.example"
    local env_dst="$FREEMAPI_DIR/.env"

    if [[ ! -f "$env_dst" ]]; then
        run_cmd cp "$env_src" "$env_dst"
        run_shell "openssl rand -hex 32 | xargs -I{} sed -i 's/^ENCRYPTION_KEY=.*/ENCRYPTION_KEY={}/' '$env_dst'"
        run_cmd chmod 600 "$env_dst"
    fi
}

# ---------------------------------------------------------------------------
# Stow dotfiles
# ---------------------------------------------------------------------------

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
        foot
        fuzzel
        git
        gtk
        hermes
        hyprland
        nvim
        opencode
        pmux
        scripts
        shell
        tmux
        waybar
        xdg
    )

    for pkg in "${packages[@]}"; do
        stow_package_force "$pkg"
    done

    # The hyprland package excludes .local from stow (see
    # hyprland/.stow-local-ignore — pre-existing absolute-symlink
    # entries in the scripts package abort sibling-tree analysis).
    # Symlink each helper script into ~/.local/bin/ manually so it's
    # on $PATH.
    if [[ -d "$DOTFILES_DIR/hyprland/.local/bin" ]]; then
        run_cmd mkdir -p "$HOME/.local/bin"
        for helper in "$DOTFILES_DIR"/hyprland/.local/bin/*; do
            [[ -f "$helper" ]] || continue
            run_cmd ln -sfr "$helper" "$HOME/.local/bin/$(basename "$helper")"
        done
    fi

    run_cmd fc-cache -f

    # Default hyprland-colors.css — waybar imports this for runtime
    # color overrides (generated by hyprland-pywal). Create the stub so
    # waybar doesn't crash on first boot when the file is missing.
    if [[ ! -f "$HOME/.config/waybar/hyprland-colors.css" ]]; then
        run_cmd mkdir -p "$HOME/.config/waybar"
        run_cmd sh -c 'cat > "$HOME/.config/waybar/hyprland-colors.css" << "CSSEOF"
/* Default accent palette — matches kara. Overwritten by hyprland-pywal. */
@define-color accent     #8fd3d3;
@define-color accent_dim #6bacac;
@define-color warn       #ebcb8b;
@define-color err        #bf616a;
CSSEOF'
    fi

    # Install init scripts (OpenRC → /etc/init.d/, systemd → /etc/systemd/system/).
    # stow targets $HOME but these must live under /etc.
    svc_install_init_scripts
}

# ---------------------------------------------------------------------------
# Post-install configuration
# ---------------------------------------------------------------------------

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

enable_bluetooth() {
    svc_enable_start bluetooth
}

tailscale_post() {
    if tailscale status >/dev/null 2>&1; then
        return 0
    fi

    warn "Tailscale is installed but not authenticated yet"

    if whiptail_run --title "Tailscale" --yesno \
"Run 'sudo tailscale up' now?\n\nThis will begin login/auth." 12 72; then
        if [[ "$DRY_RUN" -eq 0 ]]; then
            sudo tailscale up || warn "tailscale up did not complete successfully"
        fi
    else
        warn "Skipped 'tailscale up'. Run it manually later."
    fi
}

tampermonkey_reminder() {
    local zip="$DOTFILES_DIR/assets/tampermonkey/tampermonkey-backup.zip"
    [[ -f "$zip" ]] || return 0

    whiptail_run --title "Tampermonkey reminder" --msgbox \
"Tampermonkey backup detected:\n$zip\n\nImport it manually inside Floorp:\n1) Open Floorp\n2) Open Tampermonkey dashboard\n3) Utilities -> Import\n4) Select the zip file" 18 100
}

freellmapi_post() {
    local freemapi_dir="$HOME/repos/freellmapi"

    if [[ -d "$freemapi_dir" ]]; then
        run_shell "cd '$freemapi_dir' && docker compose up -d"

        # Wait for container to come up and DB to be created
        local container=""
        for i in 1 2 3 4 5 6 7 8 9 10; do
            container=$(docker ps --format '{{.Names}}' | grep freellmapi || true)
            [[ -n "$container" ]] && break
            sleep $((3 + i / 3))
        done

        local key=""
        if [[ -n "$container" ]]; then
            for i in 1 2 3 4 5 6 7 8; do
                key=$(docker exec "$container" sh -c '
                    node -e "
                        const Database = require('"'"'better-sqlite3'"'"');
                        const db = new Database('"'"'/app/server/data/freeapi.db'"'"');
                        try {
                            const row = db.prepare(\"SELECT value FROM settings WHERE key='"'"'unified_api_key'"'"'\").get();
                            if (row) console.log(row.value);
                        } catch(e) {}
                    "' 2>/dev/null || true)
                [[ -n "$key" ]] && break
                sleep $((2 + i))
            done
        fi

        if [[ -n "$key" ]]; then
            # Escape sed-special chars in key (&, /, \, newline)
            local key_escaped
            key_escaped=$(printf '%s\n' "$key" | sed -e 's/[\/&]/\\&/g')

            local opencode_config="$HOME/.config/opencode/opencode.json"
            if [[ -f "$opencode_config" ]]; then
                run_shell "sed -i \"s/__FREEFLLMAPI_API_KEY__/$key_escaped/\" '$opencode_config'"
                log "FreeLLMAPI API key synced to opencode config"
            fi
            local hermes_env="$HOME/.hermes/.env"
            if [[ -f "$hermes_env" ]]; then
                run_shell "sed -i \"s/^FREEFLLMAPI_API_KEY=.*/FREEFLLMAPI_API_KEY=$key_escaped/\" '$hermes_env'"
                log "FreeLLMAPI API key synced to Hermes .env"
            fi
        else
            warn "Could not retrieve FreeLLMAPI API key — sync it manually after install"
        fi
    fi

    if [[ "$INIT_SYSTEM" == "systemd" ]] && [[ -f /etc/systemd/system/freellmapi@.service ]]; then
        svc_enable_start "freellmapi@$USER" 2>/dev/null || true
    elif [[ -f /etc/init.d/freellmapi ]]; then
        svc_enable_start freellmapi 2>/dev/null || true
    fi
}

hermes_post() {
    local env_file="$HOME/.hermes/.env"
    [[ -f "$env_file" ]] || return 0

    if grep -q 'YOUR_OPENROUTER_API_KEY_HERE\|sk-or-v1-xxx' "$env_file" 2>/dev/null; then
        warn "Hermes API keys not configured"
        whiptail_run --title "Hermes API Keys" --msgbox \
"Edit ~/.hermes/.env and add your API keys:

  OPENROUTER_API_KEY   — required (all LLM routes through OpenRouter)
  BRAVE_SEARCH_API_KEY  — required for web search
  FREEFLLMAPI_API_KEY   — required for free-tier (Kimi K2.6)

Get keys at:
  https://openrouter.ai/keys
  https://brave.com/search/api/

After saving, verify with: hermes model --list" 20 80
    fi
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
- Run: start-hyprland
- If Tailscale is not connected yet, run: sudo tailscale up
- Run: docker run hello-world after re-login to confirm Docker group access
- Open Floorp and manually import the Tampermonkey backup if you want it
- Edit ~/.hermes/.env to add your API keys if prompted
- Run: hermes model --list  (verify Hermes is working)
- Run: hermes-setup --all   (optional: gateway cron for kanban dispatch)
- FreeLLMAPI is running at localhost:3001 (API key synced to opencode/hermes configs)
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

    whiptail_run --title "Install complete" --msgbox "$notes\\nLog:\\n$LOG_FILE" 28 110
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

main() {
    : >"$LOG_FILE"
    log "=== artix bootstrap start (dry_run=$DRY_RUN) ==="

    require_arch
    check_prerequisites
    ensure_sudo
    setup_git_config
    ensure_whiptail
    detect_terminal_ui
    ensure_dirs
    optional_prompt

    [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles directory not found: $DOTFILES_DIR"

    start_gauge

    install_base_packages

    step 44 "Installing paru (AUR helper)"
    ensure_paru

    step 45 "Installing AUR zsh plugins"
    paru_install zsh-autosuggestions zsh-fast-syntax-highlighting

    step 46 "Installing Floorp"
    install_floorp

    step 50 "Installing Tailscale"
    install_tailscale

    step 53 "Installing Docker"
    install_docker

    step 56 "Installing Neovim nightly"
    install_neovim_nightly

    step 58 "Installing swayosd (HUD daemon)"
    install_swayosd

    step 59 "Installing xdg-desktop-portal-termfilechooser (lf for GTK file dialogs)"
    install_termfilechooser

    step 59 "Installing hyprland-share-picker-preview (visual screencast picker)"
    install_hyprland_share_picker_preview

    step 60 "Enabling Bluetooth"
    enable_bluetooth

    step 73 "Installing pmux"
    install_pmux

    # TODO: uncomment when loom-rs is packaged
    # step 78 "Installing loom"
    # install_loom

    step 76 "Installing Hermes agent"
    install_hermes

    step 80 "Stowing dotfiles"
    stow_dotfiles

    step 77 "Installing FreeLLMAPI (free LLM proxy)"
    install_freellmapi

    step 78 "Starting FreeLLMAPI"
    freellmapi_post

    step 90 "Setting default shell to zsh"
    ensure_zsh_default_shell

    step 92 "Applying default theme"
    apply_default_theme

    if [[ "$INSTALL_SUNSHINE" -eq 1 ]]; then
        step 94 "Installing Sunshine"
        install_sunshine
    fi

    if [[ "$INSTALL_STEAM" -eq 1 ]]; then
        step 96 "Installing Steam"
        install_steam
    fi

    if [[ "$INSTALL_MULLVAD" -eq 1 ]]; then
        step 98 "Installing Mullvad"
        install_mullvad
    fi

    if [[ "$INSTALL_GAMING" -eq 1 ]]; then
        step 99 "Installing gaming tweaks"
        install_gaming_tweaks
    fi

    finish_gauge

    tailscale_post
    tampermonkey_reminder
    hermes_post
    post_install_notes

    log "=== artix bootstrap end ==="
}

# ---------------------------------------------------------------------------
# Ensure sudo is available and credentials are cached before main()
# ---------------------------------------------------------------------------
check_sudo_early() {
    command -v sudo >/dev/null 2>&1 || { echo "[FATAL] sudo not found" >&2; exit 1; }

    # Check if credentials are already cached
    if sudo -n true 2>/dev/null; then
        echo "[setup] sudo credentials already cached." >&2
        return 0
    fi

    echo "[setup] This installer requires sudo privileges." >&2
    echo "[sudo] Enter your password when prompted (this prompt appears directly on your terminal)." >&2

    if ! sudo -v; then
        echo "[FATAL] sudo authentication failed. Cannot continue." >&2
        echo "  Run 'sudo -v' manually to verify, then re-run 'make install'." >&2
        exit 1
    fi

    echo "[setup] sudo authenticated." >&2
}

check_sudo_early
# Start the keepalive loop now so all future sudo -n calls succeed
( while true; do sudo -n true; sleep 60; done ) 2>/dev/null &
SUDO_KEEPALIVE_PID=$!
trap 'kill "${SUDO_KEEPALIVE_PID:-0}" 2>/dev/null || true' EXIT

main "$@"
