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

GAUGE_FD_OPEN=""

# Hardware selections (set by TUI)
CPU_CHOICE="auto"   # auto|amd|intel
GPU_CHOICE="auto"   # auto|amd|intel|nvidia
SUDO_KEEPALIVE_PID=""

log() { printf "%s\n" "$*" >> "$LOG_FILE"; }

die_ui() {
    local msg="$1"
    if [[ -n "${GAUGE_FD_OPEN:-}" ]]; then
        exec 4>&- || true
        GAUGE_FD_OPEN=""
    fi
    whiptail --title "Error" --msgbox "$msg\n\nLog:\n$LOG_FILE" 18 96
    if whiptail --title "View Log" --yesno "Open the install log now?" 10 60; then
        whiptail --title "Install Log" --textbox "$LOG_FILE" 28 120
    fi
    exit 1
}

trap 'die_ui "Failed at line $LINENO:\n$BASH_COMMAND\n\nLast log lines:\n$(tail -n 160 "$LOG_FILE" 2>/dev/null || true)"' ERR

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
    if command -v whiptail >/dev/null 2>&1; then
        return
    fi
    sudo apt-get update -y >>"$LOG_FILE" 2>&1
    sudo apt-get install -y whiptail ca-certificates curl >>"$LOG_FILE" 2>&1
}

start_gauge() {
    exec 4> >(whiptail --title "Ubuntu dwm Installer" \
        --backtitle "Installing… (log: ~/.local/state/dotfiles-installer/install.log)" \
        --gauge "Preparing…" 12 96 0)
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

ensure_dirs() {
    run_cmd mkdir -p "$HOME/.config" "$HOME/.local/bin" "$HOME/repos" "$HOME/Downloads"
}

# ============================================================
# Hardware selection (TUI)
# ============================================================

detect_cpu_vendor() {
    if grep -qi "AuthenticAMD" /proc/cpuinfo 2>/dev/null; then
        echo "amd"
    elif grep -qi "GenuineIntel" /proc/cpuinfo 2>/dev/null; then
        echo "intel"
    else
        echo "unknown"
    fi
}

detect_gpu_vendor() {
    if ! command -v lspci >/dev/null 2>&1; then
        echo "unknown"
        return
    fi
    local out
    out="$(lspci -nn 2>/dev/null | grep -Ei "VGA|3D|Display" || true)"
    if grep -qi "NVIDIA" <<<"$out"; then
        echo "nvidia"
    elif grep -qi "AMD|ATI" <<<"$out"; then
        echo "amd"
    elif grep -qi "Intel" <<<"$out"; then
        echo "intel"
    else
        echo "unknown"
    fi
}

hardware_prompt() {
    # Best-effort detection for defaults (doesn't block)
    local cpu_guess="auto"
    local gpu_guess="auto"

    if command -v grep >/dev/null 2>&1; then
        local c
        c="$(detect_cpu_vendor)"
        [[ "$c" == "amd" || "$c" == "intel" ]] && cpu_guess="$c"
    fi

    if command -v lspci >/dev/null 2>&1; then
        local g
        g="$(detect_gpu_vendor)"
        [[ "$g" == "amd" || "$g" == "intel" || "$g" == "nvidia" ]] && gpu_guess="$g"
    fi

    CPU_CHOICE="$(whiptail --title "Hardware: CPU" --radiolist \
"Select CPU vendor (affects microcode package):" 14 84 4 \
"auto"  "Auto-detect (recommended) [detected: ${cpu_guess}]" ON \
"amd"   "AMD" OFF \
"intel" "Intel" OFF \
"skip"  "Skip microcode install" OFF \
3>&1 1>&2 2>&3)" || die_ui "Aborted."

    GPU_CHOICE="$(whiptail --title "Hardware: GPU" --radiolist \
"Select GPU vendor (affects optional driver step):" 14 84 5 \
"auto"   "Auto-detect (recommended) [detected: ${gpu_guess}]" ON \
"amd"    "AMD" OFF \
"intel"  "Intel" OFF \
"nvidia" "NVIDIA" OFF \
"skip"   "Skip GPU vendor selection" OFF \
3>&1 1>&2 2>&3)" || die_ui "Aborted."

    log "[hw] CPU_CHOICE=$CPU_CHOICE GPU_CHOICE=$GPU_CHOICE (guesses: cpu=$cpu_guess gpu=$gpu_guess)"
}

# ============================================================
# Base install
# ============================================================

install_base_packages() {
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get upgrade -y

    run_cmd sudo apt-get install -y \
        git curl ca-certificates wget unzip fontconfig \
        stow \
        build-essential pkg-config gcc make \
        zsh tmux fzf tree ripgrep fd-find \
        xclip playerctl flameshot \
        dunst libnotify-bin picom unclutter sxhkd xwallpaper \
        blueman light \
        gnupg python3 software-properties-common \
        libx11-dev libxinerama-dev libxft-dev \
        x11-xserver-utils dbus-x11 \
        xinit xserver-xorg-core \
        linux-firmware pciutils usbutils lm-sensors rfkill \
        mesa-utils vulkan-tools libvulkan1 mesa-vulkan-drivers

    # Needed for GPU detection later, harmless everywhere
    run_cmd sudo apt-get install -y pciutils

    install_cpu_microcode
}

install_cpu_microcode() {
    # Microcode improves stability/security on real hardware. Idempotent.
    local vendor="$CPU_CHOICE"
    if [[ "$vendor" == "auto" ]]; then
        vendor="$(detect_cpu_vendor)"
    fi

    case "$vendor" in
        amd)
            run_cmd sudo apt-get install -y amd64-microcode
            log "[hw] installed amd64-microcode"
            ;;
        intel)
            run_cmd sudo apt-get install -y intel-microcode
            log "[hw] installed intel-microcode"
            ;;
        skip|unknown|*)
            log "[hw] skipping microcode install (vendor=$vendor)"
            ;;
    esac
}

# Optional NVIDIA driver installer (disabled-by-default menu item)
install_nvidia_proprietary_driver() {
    # This uses Ubuntu's recommended driver selection.
    # If Secure Boot is enabled, you'll get MOK enrollment prompts.
    local vendor="$GPU_CHOICE"
    if [[ "$vendor" == "auto" ]]; then
        vendor="$(detect_gpu_vendor)"
    fi

    if [[ "$vendor" != "nvidia" ]]; then
        whiptail --title "NVIDIA Driver" --msgbox \
"GPU selection is not NVIDIA (current: $vendor).\n\nSkipping NVIDIA proprietary driver install." \
12 80
        log "[hw] nvidia driver skipped (vendor=$vendor)"
        return 0
    fi

    whiptail --title "NVIDIA Driver" --yesno \
"This will install the recommended proprietary NVIDIA driver using 'ubuntu-drivers autoinstall'.\n\nNotes:\n- If Secure Boot is ON, you may be prompted to enroll a key (MOK).\n- Reboot required.\n\nProceed?" \
18 96 || { log "[hw] user cancelled nvidia driver install"; return 0; }

    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y ubuntu-drivers-common

    run_cmd sudo ubuntu-drivers autoinstall
    log "[hw] ran ubuntu-drivers autoinstall"
}

install_neovim_ppa() {
    run_cmd sudo add-apt-repository ppa:neovim-ppa/unstable
    run_cmd sudo apt update
    run_cmd sudo apt install -y neovim
}

install_wezterm() {
    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

    run_shell "curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /usr/share/keyrings/wezterm-fury.gpg"
    run_shell "echo 'deb [signed-by=/usr/share/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list >/dev/null"
    run_cmd sudo chmod 644 /usr/share/keyrings/wezterm-fury.gpg
    run_cmd sudo apt update
    run_cmd sudo apt install -y wezterm
}

install_floorp() {
    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

    run_shell "curl -fsSL https://ppa.floorp.app/KEY.gpg | sudo gpg --dearmor -o /usr/share/keyrings/Floorp.gpg"
    run_cmd sudo curl -sS --compressed -o /etc/apt/sources.list.d/Floorp.list "https://ppa.floorp.app/Floorp.list"
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y floorp
}

install_tailscale() {
    run_shell "curl -fsSL https://tailscale.com/install.sh | sh"
}

github_latest_asset_url() {
    local owner="$1"
    local repo="$2"
    local regex="$3"
    local api="https://api.github.com/repos/${owner}/${repo}/releases/latest"

    python3 - "$regex" <<'PY' < <(curl -fsSL "$api")
import json, re, sys
regex = sys.argv[1]
data = json.load(sys.stdin)
assets = data.get("assets", [])
pat = re.compile(regex)
for a in assets:
    url = a.get("browser_download_url", "")
    name = a.get("name", "")
    if url and (pat.search(url) or pat.search(name)):
        print(url)
        sys.exit(0)

print("__ASSETS__")
for a in assets:
    print(a.get("name", ""))
sys.exit(1)
PY
}

install_nerd_font_firacode() {
    run_cmd mkdir -p "$HOME/.local/share/fonts"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] download latest Nerd Fonts FiraCode.zip and install to ~/.local/share/fonts + fc-cache"
        return 0
    fi

    local out
    out="$(github_latest_asset_url "ryanoasis" "nerd-fonts" "FiraCode\\.zip$" 2>>"$LOG_FILE")" || true

    if [[ -z "$out" ]]; then
        die_ui "Could not resolve Nerd Fonts FiraCode.zip from GitHub latest release (see log)."
    fi

    if grep -q "^__ASSETS__$" <<<"$out"; then
        log "[nerd-fonts] asset listing:"
        printf "%s\n" "$out" >>"$LOG_FILE"
        die_ui "Could not find FiraCode.zip in Nerd Fonts latest release. (Asset names were logged.)"
    fi

    local url="$out"
    local tmp
    tmp="$(mktemp -d)"

    run_shell "cd '$tmp' && curl -fL --retry 3 --retry-delay 2 -o FiraCode.zip '$url' && unzip -q FiraCode.zip"
    run_shell "find '$tmp' -type f \\( -iname '*.ttf' -o -iname '*.otf' \\) -print0 | xargs -0 -I{} cp -f '{}' '$HOME/.local/share/fonts/'"
    run_cmd rm -rf "$tmp"
    run_cmd fc-cache -f
}

install_deb_from_url() {
    local url="$1"
    local name="$2"
    local tmp
    tmp="$(mktemp -d)"
    local deb="$tmp/pkg.deb"

    log "Downloading $name from $url"
    run_shell "curl -fL --retry 3 --retry-delay 2 -o '$deb' '$url'"

    run_shell "sudo dpkg -i '$deb' || true"
    run_cmd sudo apt-get -f install -y

    run_cmd rm -rf "$tmp"
}

install_moonlight_latest() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Moonlight from latest GitHub release .deb"
        return 0
    fi

    local out
    out="$(github_latest_asset_url "moonlight-stream" "moonlight-qt" "(amd64|x86_64).*\\.deb$" 2>>"$LOG_FILE")" || true
    if grep -q "^__ASSETS__$" <<<"$out" || [[ -z "$out" ]]; then
        log "[moonlight] asset listing:"
        printf "%s\n" "$out" >>"$LOG_FILE"
        die_ui "Could not find Moonlight .deb in latest GitHub release. (Asset names were logged.)"
    fi
    install_deb_from_url "$out" "Moonlight"
}

install_sunshine_latest() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Sunshine from latest GitHub release .deb"
        return 0
    fi

    local out
    out="$(github_latest_asset_url "LizardByte" "Sunshine" "(amd64|x86_64).*\\.deb$" 2>>"$LOG_FILE")" || true
    if grep -q "^__ASSETS__$" <<<"$out" || [[ -z "$out" ]]; then
        log "[sunshine] asset listing:"
        printf "%s\n" "$out" >>"$LOG_FILE"
        die_ui "Could not find Sunshine .deb in latest GitHub release. (Asset names were logged.)"
    fi
    install_deb_from_url "$out" "Sunshine"
}

sunshine_post_config() {
    run_cmd mkdir -p "$HOME/.config/sunshine/credentials"
    run_cmd chmod 700 "$HOME/.config/sunshine/credentials"
}

install_steam() {
    run_cmd sudo dpkg --add-architecture i386
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y steam
}

install_plex_server() {
    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_shell "curl -fsSL https://downloads.plex.tv/plex-keys/PlexSign.key | sudo gpg --dearmor -o /usr/share/keyrings/plex-archive-keyring.gpg"
    run_shell "echo 'deb [signed-by=/usr/share/keyrings/plex-archive-keyring.gpg] https://downloads.plex.tv/repo/deb public main' | sudo tee /etc/apt/sources.list.d/plex.list >/dev/null"
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y plexmediaserver
}

stow_backup_conflicts() {
    local stow_out="$1"
    local ts
    ts="$(date +%Y%m%d-%H%M%S)"

    mapfile -t targets < <(
        awk '
            /existing target is neither a link nor a directory:/ {
                for (i=1; i<=NF; i++) if ($i ~ /directory:/) { print $(i+1) }
            }
        ' "$stow_out"
    )

    if [[ "${#targets[@]}" -eq 0 ]]; then
        return 0
    fi

    for rel in "${targets[@]}"; do
        local abs="$HOME/$rel"
        if [[ -e "$abs" && ! -L "$abs" ]]; then
            local bak="${abs}.bak-${ts}"
            log "[stow] backing up: $abs -> $bak"
            run_cmd mv "$abs" "$bak"
        fi
    done
}

stow_dotfiles() {
    [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles dir not found: $DOTFILES_DIR"
    ensure_dirs

    local modules=(shell nvim tmux wezterm scripts xinit-desktop floorp git xresources assets sunshine)

    local tmp
    tmp="$(mktemp)"
    (cd "$DOTFILES_DIR" && stow -n -v -t "$HOME" "${modules[@]}") >"$tmp" 2>&1 || true

    if grep -qi "would cause conflicts" "$tmp"; then
        log "Stow conflicts detected:"
        sed 's/^/[stow] /' "$tmp" >>"$LOG_FILE"

        if [[ "$DRY_RUN" -eq 1 ]]; then
            whiptail --title "Stow Conflicts (Dry Run)" --msgbox \
"Stow reports conflicts.\n\nDry run will not modify anything.\n\nSee log:\n$LOG_FILE" 14 96
            whiptail --title "Stow Output" --textbox "$tmp" 28 120
            rm -f "$tmp"
            return 0
        fi

        whiptail --title "Stow Conflicts" --textbox "$tmp" 28 120

        local choice
        choice="$(whiptail --title "Resolve Conflicts" --radiolist \
"Choose a conflict strategy:" 16 96 3 \
"backup" "Backup conflicting files (recommended) and continue" ON \
"adopt" "Use stow --adopt (risky: rewrites your stow tree)" OFF \
"abort" "Abort and fix conflicts manually" OFF \
3>&1 1>&2 2>&3)" || die_ui "Aborted."

        case "$choice" in
            backup)
                stow_backup_conflicts "$tmp"
                rm -f "$tmp"
                (cd "$DOTFILES_DIR" && run_cmd stow -v -t "$HOME" "${modules[@]}")
                ;;
            adopt)
                rm -f "$tmp"
                (cd "$DOTFILES_DIR" && run_cmd stow --adopt -v -t "$HOME" "${modules[@]}")
                ;;
            *)
                rm -f "$tmp"
                die_ui "Aborted due to stow conflicts."
                ;;
        esac
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

link_suckless_into_repos() {
    ensure_dirs
    for t in dwm dmenu slstatus; do
        if [[ -e "$HOME/repos/$t" || -L "$HOME/repos/$t" ]]; then
            run_cmd rm -rf "$HOME/repos/$t"
        fi
    done

    run_cmd ln -s "$DOTFILES_DIR/suckless/dwm" "$HOME/repos/dwm"
    run_cmd ln -s "$DOTFILES_DIR/suckless/dmenu" "$HOME/repos/dmenu"
    run_cmd ln -s "$DOTFILES_DIR/suckless/slstatus" "$HOME/repos/slstatus"
}

build_install_suckless() {
    for t in dwm dmenu slstatus; do
        [[ -d "$HOME/repos/$t" ]] || die_ui "Missing $HOME/repos/$t. Did linking fail?"
        run_shell "cd '$HOME/repos/$t' && make clean && make"
        run_shell "cd '$HOME/repos/$t' && sudo make install"
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

floorp_find_install_default_profile_relpath() {
    local ini="$HOME/.floorp/profiles.ini"
    [[ -f "$ini" ]] || { echo ""; return 0; }

    local install_default
    install_default="$(awk -F= '
        /^\[Install/ {in_install=1; next}
        /^\[/ {in_install=0}
        in_install && $1=="Default" {print $2; exit}
    ' "$ini" | head -n1)"

    if [[ -n "$install_default" ]]; then
        echo "$install_default"
        return 0
    fi

    local profile_default
    profile_default="$(awk -F= '
        /^\[Profile/ {p=1; path=""; def=0; next}
        /^\[/ {p=0}
        p && $1=="Path" {path=$2}
        p && $1=="Default" {def=$2}
        p && def==1 && path!="" {print path; exit}
    ' "$ini" | head -n1)"

    echo "$profile_default"
}

floorp_apply_template() {
    local tpl="$DOTFILES_DIR/floorp/.config/floorp/profile-template"
    [[ -d "$tpl" ]] || { log "No Floorp template at $tpl; skipping."; return 0; }

    local rel
    rel="$(floorp_find_install_default_profile_relpath)"
    if [[ -z "$rel" ]]; then
        log "No Floorp profiles.ini default profile found; skipping apply."
        return 0
    fi

    local profile="$HOME/.floorp/$rel"
    [[ -d "$profile" ]] || die_ui "Floorp profile directory not found: $profile"

    if [[ -f "$tpl/user.js" ]]; then
        run_cmd cp -f "$tpl/user.js" "$profile/user.js"
    fi

    if [[ -d "$tpl/chrome" ]]; then
        run_cmd mkdir -p "$profile/chrome"
        run_cmd cp -a "$tpl/chrome/." "$profile/chrome/"
    fi

    log "Applied Floorp template to: $profile"
}

tampermonkey_prompt_import() {
    local zip="$DOTFILES_DIR/assets/tampermonkey/tampermonkey-backup.zip"
    if [[ ! -f "$zip" ]]; then
        log "Tampermonkey zip not found at $zip; skipping prompt."
        return 0
    fi

    whiptail --title "Tampermonkey Import" --msgbox \
"Tampermonkey data lives inside the extension and cannot be safely auto-imported by a script.

Backup detected:
$zip

Do this after install:
1) Open Floorp
2) Open Tampermonkey Dashboard
3) Utilities -> Import
4) Select: $zip" 18 96

    if whiptail --title "Open Folder" --yesno "Open the folder containing the backup zip now?" 10 72; then
        if command -v xdg-open >/dev/null 2>&1; then
            run_cmd xdg-open "$(dirname "$zip")"
        fi
    fi
}

set_default_shell_zsh() {
    [[ "${SHELL:-}" == "/bin/zsh" ]] && return 0
    run_cmd chsh -s /bin/zsh "$USER" || true
}

# ============================================================
# Corporate Enrollment (Intune + Defender for Endpoint)
# ============================================================

install_microsoft_apt_repo() {
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y curl ca-certificates gnupg apt-transport-https lsb-release

    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

    run_shell "curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | sudo gpg --dearmor -o /usr/share/keyrings/microsoft.gpg"
    run_cmd sudo chmod 0644 /usr/share/keyrings/microsoft.gpg

    local rel codename
    rel="$(lsb_release -rs)"
    codename="$(lsb_release -cs)"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] write microsoft apt repo for ubuntu ${rel} (${codename})"
        return 0
    fi

    sudo tee /etc/apt/sources.list.d/microsoft-ubuntu-prod.list >/dev/null <<EOF
deb [arch=amd64 signed-by=/usr/share/keyrings/microsoft.gpg] https://packages.microsoft.com/ubuntu/${rel}/prod ${codename} main
EOF

    run_cmd sudo apt-get update -y
}

install_edge_intune_mde_packages() {
    run_cmd sudo apt-get update -y

    run_cmd sudo apt-get install -y microsoft-edge-stable
    run_cmd sudo apt-get install -y intune-portal

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install mdatp and run health check"
        return 0
    fi

    if ! sudo apt-get install -y mdatp >>"$LOG_FILE" 2>&1; then
        die_ui "Failed to install Defender for Endpoint (mdatp) from Microsoft repo.\n\nSee log:\n$LOG_FILE"
    fi

    if command -v mdatp >/dev/null 2>&1; then
        mdatp health >>"$LOG_FILE" 2>&1 || true
    fi
}

prompt_defender_onboarding() {
    local msg
    msg="Defender onboarding is TENANT-SPECIFIC.\n\nDownload your onboarding script (.py) from your Defender portal.\n\nIf you already downloaded it to this machine, you can provide its path now and the installer will run it.\n\nRun onboarding now?"
    if ! whiptail --title "Defender Onboarding" --yesno "$msg" 18 96; then
        log "[corp] user skipped defender onboarding prompt"
        return 0
    fi

    local path=""
    path="$(whiptail --title "Onboarding Script Path" --inputbox \
"Enter the full path to the Defender onboarding script (.py)\n\nExample:\n/home/$USER/Downloads/MicrosoftDefenderATPOnboardingLinuxServer.py" \
14 96 "" 3>&1 1>&2 2>&3)" || { log "[corp] onboarding path input cancelled"; return 0; }

    if [[ -z "$path" ]]; then
        log "[corp] empty onboarding path; skipping"
        return 0
    fi

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] run python3 '$path' then mdatp health"
        return 0
    fi

    if [[ ! -f "$path" ]]; then
        whiptail --title "Onboarding Script Not Found" --msgbox \
"File not found:\n$path\n\nSkipping onboarding.\nYou can run it later manually:\npython3 <path-to-onboarding.py>" \
14 96
        log "[corp] onboarding file not found: $path"
        return 0
    fi

    run_shell "python3 '$path'"

    if command -v mdatp >/dev/null 2>&1; then
        run_shell "mdatp health || true"
    fi

    whiptail --title "Defender Onboarding" --msgbox \
"Onboarding script executed.\n\nVerify:\n- Device appears in Defender portal\n- 'mdatp health' looks good\n\nLog:\n$LOG_FILE" \
16 96
}

intune_enrollment_instructions() {
    whiptail --title "Intune Enrollment Steps" --msgbox \
"Intune enrollment requires an interactive sign-in.\n\nNotes:\n- You can keep using Floorp daily.\n- Edge is installed ONLY to satisfy enrollment requirements.\n- Your WM (dwm/.xinitrc) is fine AFTER enrollment.\n\nDo this once:\n1) Log into a session where GUI apps can be launched.\n2) Run: intune-portal\n3) Sign in with work account.\n4) Complete enrollment prompts.\n\nAfter enrollment:\n- Go back to dwm/.xinitrc.\n- Leave Edge installed.\n\nLog:\n$LOG_FILE" \
22 104
}

# ============================================================
# Menu
# ============================================================

main_menu() {
    whiptail --title "Ubuntu dwm Installer" --checklist "Select what to install:" 28 104 20 \
        "packages" "Install system packages (X + startx + build deps + firmware + microcode)" ON \
        "stow" "Stow dotfiles into HOME (backs up conflicts if needed)" ON \
        "wezterm" "Install WezTerm (apt.fury.io/wez)" ON \
        "neovim" "Install Neovim via PPA (neovim-ppa/unstable)" ON \
        "tailscale" "Install Tailscale (official script)" ON \
        "floorp" "Install Floorp browser (ppa.floorp.app)" ON \
        "floorp_profile" "Apply Floorp UI template (user.js + chrome/)" ON \
        "tampermonkey" "Show Tampermonkey import steps (if backup zip exists)" ON \
        "moonlight" "Install Moonlight (latest GitHub .deb)" ON \
        "sunshine" "Install Sunshine (latest GitHub .deb)" ON \
        "steam" "Install Steam (APT, enables i386 multiarch)" OFF \
        "plex" "Install Plex Media Server (APT repo)" OFF \
        "suckless" "Init submodules + symlink into ~/repos + build+install dwm/dmenu/slstatus" ON \
        "session" "Register dwm in GDM login sessions" ON \
        "fonts" "Install Nerd Fonts: FiraCode" ON \
        "shell" "Set default shell to zsh" ON \
        "nvidia_driver" "GPU: Install NVIDIA proprietary driver (ubuntu-drivers autoinstall)" OFF \
        "corp_enroll" "WORK ENROLLMENT: Install Edge + Intune Portal + Defender (manual sign-in required)" OFF \
        3>&1 1>&2 2>&3
}

# ============================================================
# Main
# ============================================================

main() {
    : >"$LOG_FILE"
    log "=== dotfiles installer start (dry_run=$DRY_RUN) ==="

    require_ubuntu
    ensure_sudo
    ensure_whiptail

    [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles directory not found: $DOTFILES_DIR"

    local selected
    selected="$(main_menu)" || exit 1

    # Hardware prompt only if it matters for requested steps
    if grep -q "\"packages\"" <<<"$selected" || grep -q "\"nvidia_driver\"" <<<"$selected"; then
        hardware_prompt
    fi

    start_gauge

    local ran=()
    local pct=0

    ensure_dirs

    if grep -q "\"packages\"" <<<"$selected"; then
        pct=5; gauge $pct "Installing system packages…"
        install_base_packages
        ran+=("packages")
    fi

    if grep -q "\"stow\"" <<<"$selected"; then
        pct=15; gauge $pct "Stowing dotfiles…"
        stow_dotfiles
        ran+=("stow")
    fi

    if grep -q "\"wezterm\"" <<<"$selected"; then
        pct=22; gauge $pct "Installing WezTerm…"
        install_wezterm
        ran+=("wezterm")
    fi

    if grep -q "\"neovim\"" <<<"$selected"; then
        pct=30; gauge $pct "Installing Neovim (PPA)…"
        install_neovim_ppa
        ran+=("neovim")
    fi

    if grep -q "\"tailscale\"" <<<"$selected"; then
        pct=38; gauge $pct "Installing Tailscale…"
        install_tailscale
        ran+=("tailscale")
    fi

    if grep -q "\"floorp\"" <<<"$selected"; then
        pct=46; gauge $pct "Installing Floorp…"
        install_floorp
        ran+=("floorp")
    fi

    if grep -q "\"floorp_profile\"" <<<"$selected"; then
        pct=52; gauge $pct "Applying Floorp UI template…"
        floorp_apply_template
        ran+=("floorp_profile")
    fi

    if grep -q "\"moonlight\"" <<<"$selected"; then
        pct=60; gauge $pct "Installing Moonlight…"
        install_moonlight_latest
        ran+=("moonlight")
    fi

    if grep -q "\"sunshine\"" <<<"$selected"; then
        pct=70; gauge $pct "Installing Sunshine…"
        install_sunshine_latest
        pct=72; gauge $pct "Preparing Sunshine runtime directories…"
        sunshine_post_config
        ran+=("sunshine")
    fi

    if grep -q "\"steam\"" <<<"$selected"; then
        pct=74; gauge $pct "Installing Steam…"
        install_steam
        ran+=("steam")
    fi

    if grep -q "\"plex\"" <<<"$selected"; then
        pct=78; gauge $pct "Installing Plex Media Server…"
        install_plex_server
        ran+=("plex")
    fi

    if grep -q "\"suckless\"" <<<"$selected"; then
        pct=82; gauge $pct "Initializing suckless submodules…"
        ensure_suckless_submodules
        pct=86; gauge $pct "Linking suckless repos into ~/repos…"
        link_suckless_into_repos
        pct=90; gauge $pct "Building and installing dwm/dmenu/slstatus…"
        build_install_suckless
        ran+=("suckless")
    fi

    if grep -q "\"session\"" <<<"$selected"; then
        pct=93; gauge $pct "Registering dwm session…"
        register_dwm_session
        ran+=("session")
    fi

    if grep -q "\"fonts\"" <<<"$selected"; then
        pct=95; gauge $pct "Installing fonts…"
        install_nerd_font_firacode
        ran+=("fonts")
    fi

    if grep -q "\"shell\"" <<<"$selected"; then
        pct=97; gauge $pct "Setting default shell…"
        set_default_shell_zsh
        ran+=("shell")
    fi

    if grep -q "\"tampermonkey\"" <<<"$selected"; then
        pct=99; gauge $pct "Tampermonkey import instructions…"
        tampermonkey_prompt_import
        ran+=("tampermonkey")
    fi

    if grep -q "\"nvidia_driver\"" <<<"$selected"; then
        pct=40; gauge $pct "Installing NVIDIA driver…"
        install_nvidia_proprietary_driver
        ran+=("nvidia_driver")
    fi

    if grep -q "\"corp_enroll\"" <<<"$selected"; then
        pct=45; gauge $pct "Installing Microsoft apt repo…"
        install_microsoft_apt_repo
        pct=60; gauge $pct "Installing Edge + Intune Portal + Defender…"
        install_edge_intune_mde_packages
        pct=70; gauge $pct "Defender onboarding (optional)…"
        prompt_defender_onboarding
        pct=75; gauge $pct "Showing Intune enrollment steps…"
        intune_enrollment_instructions
        ran+=("corp_enroll")
    fi

    finish_gauge

    local summary="Completed."
    if [[ "$DRY_RUN" -eq 1 ]]; then
        summary="Dry run completed (no changes made)."
    fi

    local ran_text
    ran_text="$(printf "%s\n" "${ran[@]:-none}" | sed 's/^/- /')"

    local corp_notes=""
    if grep -q "\"corp_enroll\"" <<<"$selected"; then
        corp_notes="
Work Enrollment Notes:
- Edge was installed to support Intune enrollment. You can keep using Floorp daily.
- Enrollment requires: run 'intune-portal' and sign in.
- Defender onboarding is tenant-specific. If skipped, run later:
  python3 /path/to/onboarding.py
  then: mdatp health"
    fi

    local hw_notes=""
    if grep -q "\"packages\"" <<<"$selected" || grep -q "\"nvidia_driver\"" <<<"$selected"; then
        hw_notes="
Hardware Notes:
- CPU selection affects microcode install (intel-microcode / amd64-microcode).
- NVIDIA proprietary driver step is optional and needs a reboot."
    fi

    whiptail --title "Installation Complete" --msgbox \
"$summary

Ran steps:
$ran_text
$hw_notes
$corp_notes

Next steps:
- If you installed dwm session: Log out -> choose 'dwm' -> log in
- If Floorp template did not apply: launch Floorp once, close it, re-run with Floorp UI template checked
- If NVIDIA driver was installed: reboot

Log file:
$LOG_FILE" 28 104

    if whiptail --title "View Log" --yesno "Open the install log now?" 10 60; then
        whiptail --title "Install Log" --textbox "$LOG_FILE" 28 120
    fi

    log "=== dotfiles installer end ==="
}

main "$@"
