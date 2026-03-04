#!/usr/bin/env bash
set -Eeuo pipefail

# Whiptail colors
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
LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles-installer"
LOG_FILE="$LOG_DIR/install.log"
mkdir -p "$LOG_DIR"

GAUGE_FD_OPEN=""
SUDO_KEEPALIVE_PID=""

# Hardware selections (set by TUI)
CPU_CHOICE="auto"   # auto|amd|intel|skip
GPU_CHOICE="auto"   # auto|amd|intel|nvidia|skip

# Gauge state
CURRENT_PCT=0
CURRENT_MSG="Preparing…"
GAUGE_TICK=0

# Non-fatal warnings bucket
WARNINGS=()

log() { printf "%s\n" "$*" >> "$LOG_FILE"; }

die_ui() {
    local msg="$1"
    if [[ -n "${GAUGE_FD_OPEN:-}" ]]; then
        exec 4>&- || true
        GAUGE_FD_OPEN=""
    fi
    whiptail --title "Error" --msgbox "$msg\n\nLog:\n$LOG_FILE" 20 104
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
    exec 4> >(whiptail --title "Ubuntu .dotfiles Installer" \
        --backtitle "Installing… (log: ~/.local/state/dotfiles-installer/install.log)" \
        --gauge "Preparing…" 14 120 0)
    GAUGE_FD_OPEN=1
}

gauge_write() {
    local pct="$1"
    local msg="$2"
    [[ -n "${GAUGE_FD_OPEN:-}" ]] || return 0
    echo "$pct" >&4
    echo "# $msg" >&4
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
    step 100 "Finalizing…"
    sleep 1
    exec 4>&-
    GAUGE_FD_OPEN=""
}

# Animated progress “heartbeat” during long-running commands
gauge_pump_while_pid() {
    local pid="$1"
    local start_ts now elapsed
    start_ts="$(date +%s)"
    while kill -0 "$pid" 2>/dev/null; do
        GAUGE_TICK=$((GAUGE_TICK+1))

        local spinner
        case $((GAUGE_TICK % 4)) in
            0) spinner="";;
            1) spinner="·";;
            2) spinner="··";;
            3) spinner="···";;
        esac

        now="$(date +%s)"
        elapsed=$((now - start_ts))

        local last_line=""
        last_line="$(tail -n 30 "$LOG_FILE" 2>/dev/null | sed '/^\s*$/d' | tail -n 1 || true)"
        if [[ -n "$last_line" ]]; then
            last_line="${last_line:0:100}"
        fi

        local msg="Step: ${CURRENT_MSG}${spinner}   Elapsed: ${elapsed}s"
        if [[ -n "$last_line" ]]; then
            msg="${msg}\nLast: ${last_line}"
        else
            msg="${msg}\nLast: (starting…)"
        fi

        gauge_write "$CURRENT_PCT" "$msg"
        sleep 2
    done
}

run_cmd() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] $*"
        return 0
    fi

    log "[cmd] $*"
    set +e
    "$@" >>"$LOG_FILE" 2>&1 &
    local pid=$!
    set -e

    if [[ -n "${GAUGE_FD_OPEN:-}" ]]; then
        gauge_pump_while_pid "$pid"
    fi

    wait "$pid" || {
        local rc=$?
        die_ui "Command failed (exit=$rc):\n$*\n\nLast log lines:\n$(tail -n 120 "$LOG_FILE" 2>/dev/null || true)"
    }
}

run_shell() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] bash -lc $1"
        return 0
    fi
    log "[shell] bash -lc $1"
    set +e
    bash -lc "set -euo pipefail; $1" >>"$LOG_FILE" 2>&1 &
    local pid=$!
    set -e

    if [[ -n "${GAUGE_FD_OPEN:-}" ]]; then
        gauge_pump_while_pid "$pid"
    fi

    wait "$pid" || {
        local rc=$?
        die_ui "Shell command failed (exit=$rc):\n$1\n\nLast log lines:\n$(tail -n 120 "$LOG_FILE" 2>/dev/null || true)"
    }
}

warn() {
    local msg="$1"
    WARNINGS+=("$msg")
    log "[warn] $msg"
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
    local cpu_guess="auto"
    local gpu_guess="auto"

    local c
    c="$(detect_cpu_vendor)"
    [[ "$c" == "amd" || "$c" == "intel" ]] && cpu_guess="$c"

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
"Select GPU vendor (only used for optional NVIDIA driver step):" 14 84 5 \
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

install_cpu_microcode() {
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

install_docker() {
    # Docker official repo (idempotent)
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y ca-certificates curl gnupg

    run_cmd sudo install -d -m 0755 /etc/apt/keyrings
    run_shell "curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor --yes -o /etc/apt/keyrings/docker.gpg"
    run_cmd sudo chmod a+r /etc/apt/keyrings/docker.gpg

    # Get UBUNTU_CODENAME from os-release if present; fallback to lsb_release
    local codename=""
    if [[ -r /etc/os-release ]]; then
        # shellcheck disable=SC1091
        . /etc/os-release
        codename="${UBUNTU_CODENAME:-${VERSION_CODENAME:-}}"
    fi
    if [[ -z "$codename" ]] && command -v lsb_release >/dev/null 2>&1; then
        codename="$(lsb_release -cs)"
    fi
    [[ -n "$codename" ]] || die_ui "Could not determine Ubuntu codename for Docker repo."

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] configure docker apt repo for $codename and install docker-ce + compose plugin; add user to docker group"
        return 0
    fi

    sudo tee /etc/apt/sources.list.d/docker.list >/dev/null <<EOF
deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu ${codename} stable
EOF

    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

    # Add current user to docker group (non-fatal if already)
    if getent group docker >/dev/null 2>&1; then
        run_cmd sudo usermod -aG docker "$USER" || true
    fi

    run_cmd sudo systemctl enable --now docker || true
    log "[docker] installed and enabled docker"
}

install_nvm() {
    # Docs method requested. Keep it simple + idempotent-ish.
    if [[ -d "$HOME/.nvm" && -f "$HOME/.nvm/nvm.sh" ]]; then
        log "[nvm] already present at ~/.nvm; skipping install"
        return 0
    fi
    run_shell "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash"
    log "[nvm] installed via official install script"
}

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
        gnupg python3 software-properties-common golang make \
        libx11-dev libxinerama-dev libxft-dev \
        x11-xserver-utils dbus-x11 \
        xinit xserver-xorg-core \
        linux-firmware pciutils usbutils lm-sensors rfkill \
        mesa-utils vulkan-tools libvulkan1 mesa-vulkan-drivers

    install_cpu_microcode

    # Your requested defaults under packages
    install_docker
    install_nvm
}

install_nvidia_proprietary_driver() {
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

    # Always overwrite keyring without prompting
    run_shell "curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /usr/share/keyrings/wezterm-fury.gpg"
    run_shell "echo 'deb [signed-by=/usr/share/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list >/dev/null"
    run_cmd sudo chmod 644 /usr/share/keyrings/wezterm-fury.gpg
    run_cmd sudo apt update
    run_cmd sudo apt install -y wezterm
}

install_floorp() {
    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

    # Fix: no interactive overwrite prompt + avoid corrupting keyring on failed downloads.
    # Write to temp first, then move into place.
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Floorp repo key + list and apt install floorp"
        return 0
    fi

    run_shell "tmp=\$(mktemp) && curl -fsSL https://ppa.floorp.app/KEY.gpg -o \"\$tmp\" && sudo gpg --dearmor --yes -o /usr/share/keyrings/Floorp.gpg \"\$tmp\" && rm -f \"\$tmp\""
    run_cmd sudo curl -fsS --compressed -o /etc/apt/sources.list.d/Floorp.list "https://ppa.floorp.app/Floorp.list"
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

    local auth_header=()
    if [[ -n "${GITHUB_TOKEN:-}" ]]; then
        auth_header=(-H "Authorization: Bearer ${GITHUB_TOKEN}")
    fi

    local tmp body status
    tmp="$(mktemp)"
    status="$(
        curl -sS -L \
          -H "Accept: application/vnd.github+json" \
          -H "User-Agent: dotfiles-installer" \
          "${auth_header[@]}" \
          -w "%{http_code}" \
          -o "$tmp" \
          "$api" || echo "000"
    )"

    if [[ "$status" == "000" ]]; then
        rm -f "$tmp"
        echo "__HTTP_ERROR__"
        echo "curl failed to reach GitHub API"
        return 1
    fi

    if [[ "$status" != "200" ]]; then
        body="$(head -c 600 "$tmp" | tr '\n' ' ' | sed 's/[[:space:]]\+/ /g')"
        rm -f "$tmp"
        echo "__HTTP_ERROR__"
        echo "GitHub API returned HTTP $status for $owner/$repo"
        echo "$body"
        return 1
    fi

    body="$(cat "$tmp")"
    rm -f "$tmp"

    python3 -c '
import json, re, sys
regex = sys.argv[1]
data = json.loads(sys.stdin.read() or "{}")
assets = data.get("assets", []) or []
pat = re.compile(regex)

for a in assets:
    name = a.get("name","") or ""
    url  = a.get("browser_download_url","") or ""
    if url and (pat.search(name) or pat.search(url)):
        print(url)
        sys.exit(0)

print("__ASSETS__")
for a in assets:
    print(a.get("name", ""))
sys.exit(1)
' "$regex" <<<"$body"
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
    # Moonlight upstream frequently does NOT ship an amd64 .deb.
    # For v6.1.0, upstream provides an x86_64 AppImage (and Snap/Flatpak).
    # This function:
    #   1) tries GitHub latest AppImage (preferred, no system packaging dependency)
    #   2) falls back to snap (if available)
    #   3) falls back to flatpak (if available / installable)

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Moonlight via GitHub AppImage; fallback to snap/flatpak if needed"
        return 0
    fi

    local url=""
    url="$(github_latest_asset_url "moonlight-stream" "moonlight-qt" "(?i)Moonlight-.*(x86_64|amd64).*\\.AppImage$" 2>>"$LOG_FILE")" || true

    if [[ -n "$url" && "$url" != "__ASSETS__"* && "$url" != "__HTTP_ERROR__"* ]]; then
        install_moonlight_appimage_from_url "$url"
        return 0
    fi

    # Log what upstream actually shipped so you can see why we didn't pick a .deb
    if [[ -n "$url" ]]; then
        warn "Moonlight GitHub release did not include an x86_64/amd64 AppImage match (details logged)."
        printf "%s\n" "$url" >>"$LOG_FILE"
    else
        warn "Moonlight GitHub lookup returned empty output; possible network/GitHub API issue (see log)."
    fi

    warn "Falling back to snap/flatpak because upstream .deb for amd64 is not available in latest GitHub release."
    install_moonlight_fallback_snap_or_flatpak
}

install_moonlight_appimage_from_url() {
    local url="$1"
    local bin_dir="$HOME/.local/bin"
    local appimage="$bin_dir/moonlight.AppImage"
    local link="$bin_dir/moonlight"

    run_cmd mkdir -p "$bin_dir"

    # Download to a temp file then atomically replace.
    local tmp
    tmp="$(mktemp -p "$bin_dir" moonlight.AppImage.XXXXXX)"

    log "[moonlight] downloading AppImage: $url"
    run_shell "curl -fL --retry 3 --retry-delay 2 -o '$tmp' '$url'"

    run_cmd chmod +x "$tmp"
    run_cmd mv -f "$tmp" "$appimage"

    # Convenience symlink so Exec=moonlight works.
    if [[ -e "$link" || -L "$link" ]]; then
        run_cmd rm -f "$link"
    fi
    run_cmd ln -s "$appimage" "$link"

    # Optional desktop entry (won't break if you're pure dwm).
    install_moonlight_desktop_entry || true

    log "[moonlight] installed AppImage to $appimage (symlink: $link)"
}

install_moonlight_desktop_entry() {
    local apps="$HOME/.local/share/applications"
    run_cmd mkdir -p "$apps"

    # Use the symlink "moonlight" so PATH is respected.
    # No icon fetching here; keep it simple and reliable.
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] write $apps/moonlight.desktop"
        return 0
    fi

    cat >"$apps/moonlight.desktop" <<'EOF'
[Desktop Entry]
Name=Moonlight
Comment=Stream games and apps from Sunshine/GeForce Experience
Exec=moonlight
Terminal=false
Type=Application
Categories=Game;Network;
EOF
    log "[moonlight] wrote desktop entry: $apps/moonlight.desktop"
}

install_moonlight_fallback_snap_or_flatpak() {
    # Prefer snap if available because it's 1 command on Ubuntu.
    # Snap store lists: `sudo snap install moonlight`.
    if command -v snap >/dev/null 2>&1; then
        log "[moonlight] installing via snap"
        run_cmd sudo snap install moonlight
        return 0
    fi

    # Flatpak fallback: Flathub app id is com.moonlight_stream.Moonlight
    # We will install flatpak if missing, add flathub remote if missing, then install.
    if ! command -v flatpak >/dev/null 2>&1; then
        log "[moonlight] flatpak not found; installing flatpak"
        run_cmd sudo apt-get update -y
        run_cmd sudo apt-get install -y flatpak
    fi

    if ! flatpak remotes --columns=name 2>/dev/null | grep -qx "flathub"; then
        log "[moonlight] adding flathub remote"
        run_cmd sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    fi

    log "[moonlight] installing via flatpak (com.moonlight_stream.Moonlight)"
    run_cmd flatpak install -y flathub com.moonlight_stream.Moonlight

    # Optional: create a shim so "moonlight" works in shells if you want it.
    # (Do not force it; flatpak exports a desktop launcher already.)
    log "[moonlight] flatpak installed; launch via app menu or: flatpak run com.moonlight_stream.Moonlight"
}

install_sunshine_latest() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Sunshine from latest GitHub release .deb"
        return 0
    fi

    local out=""

    out="$(github_latest_asset_url "LizardByte" "Sunshine" "(?i)(ubuntu|debian|linux).*(amd64|x86_64).*\\.deb$" 2>>"$LOG_FILE")" || true

    if [[ -z "$out" || "$out" == "__ASSETS__"* || "$out" == "__HTTP_ERROR__"* ]]; then
        local out2=""
        out2="$(github_latest_asset_url "LizardByte" "Sunshine" "(?i)(amd64|x86_64).*\\.deb$" 2>>"$LOG_FILE")" || true
        if [[ -n "$out2" && "$out2" != "__ASSETS__"* && "$out2" != "__HTTP_ERROR__"* ]]; then
            out="$out2"
        fi
    fi

    if [[ "$out" == "__HTTP_ERROR__"* ]]; then
        log "[sunshine] GitHub API error details:"
        printf "%s\n" "$out" >>"$LOG_FILE"
        die_ui "Sunshine download lookup failed due to GitHub API/network issue.\n\nIf this is rate limiting, export GITHUB_TOKEN and retry.\n(Details logged.)"
    fi

    if [[ -z "$out" || "$out" == "__ASSETS__"* ]]; then
        log "[sunshine] asset listing:"
        printf "%s\n" "$out" >>"$LOG_FILE"
        die_ui "Could not find a Sunshine amd64 .deb in latest GitHub release.\n(Asset names were logged.)"
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
    run_shell "curl -fsSL https://downloads.plex.tv/plex-keys/PlexSign.key | sudo gpg --dearmor --yes -o /usr/share/keyrings/plex-archive-keyring.gpg"
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

    local requested=(shell nvim tmux wezterm scripts xprofile-desktop floorp git xresources sxhkd assets sunshine)

    local modules=()
    local missing=()
    for m in "${requested[@]}"; do
        if [[ -d "$DOTFILES_DIR/$m" ]]; then
            modules+=("$m")
        else
            missing+=("$m")
        fi
    done

    if [[ "${#missing[@]}" -gt 0 ]]; then
        warn "Skipping missing stow packages: ${missing[*]}"
    fi

    if [[ "${#modules[@]}" -eq 0 ]]; then
        die_ui "No stow packages found in $DOTFILES_DIR.\nExpected directories like: shell/, nvim/, tmux/, etc."
    fi

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

ensure_zsh_default_shell() {
  local user="${SUDO_USER:-$USER}"
  local target_shell="/bin/zsh"

  echo "[shell] Ensuring $user uses $target_shell"

  # Make sure zsh exists
  if [ ! -x "$target_shell" ]; then
    echo "[shell] ERROR: $target_shell not found or not executable"
    return 1
  fi

  # Ensure it's listed in /etc/shells
  if ! grep -qx "$target_shell" /etc/shells; then
    echo "[shell] Adding $target_shell to /etc/shells"
    echo "$target_shell" | sudo tee -a /etc/shells >/dev/null
  fi

  # If already set, do nothing
  current_shell="$(getent passwd "$user" | cut -d: -f7)"
  if [ "$current_shell" = "$target_shell" ]; then
    echo "[shell] Already set"
    return 0
  fi

  # Force change via root (non-interactive safe)
  echo "[shell] Changing login shell for $user"
  sudo usermod -s "$target_shell" "$user" || {
    echo "[shell] ERROR: usermod failed"
    return 1
  }

  echo "[shell] Done. Logout/login required."
}

# ============================================================
# Corporate Enrollment (Intune + Defender for Endpoint)
# (restored exactly as a selectable step: corp_enroll)
# ============================================================

install_microsoft_apt_repo() {
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y curl ca-certificates gnupg apt-transport-https lsb-release

    run_cmd sudo install -d -m 0755 /usr/share/keyrings
    run_cmd sudo install -d -m 0755 /etc/apt/sources.list.d

    # One keyring is fine for both repos
    run_shell "curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | sudo gpg --dearmor --yes -o /usr/share/keyrings/microsoft.gpg"
    run_cmd sudo chmod 0644 /usr/share/keyrings/microsoft.gpg

    local rel codename
    rel="$(lsb_release -rs)"
    codename="$(lsb_release -cs)"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] write microsoft ubuntu prod repo (ubuntu ${rel} ${codename}) + edge repo; apt-get update"
        return 0
    fi

    # Microsoft "prod" repo (Intune/MDE packages live here depending on tenant/docs)
    sudo tee /etc/apt/sources.list.d/microsoft-ubuntu-prod.list >/dev/null <<EOF
deb [arch=amd64 signed-by=/usr/share/keyrings/microsoft.gpg] https://packages.microsoft.com/ubuntu/${rel}/prod ${codename} main
EOF

    # Microsoft Edge repo (THIS is what provides microsoft-edge-stable)
    sudo tee /etc/apt/sources.list.d/microsoft-edge.list >/dev/null <<EOF
deb [arch=amd64 signed-by=/usr/share/keyrings/microsoft.gpg] https://packages.microsoft.com/repos/edge stable main
EOF

    run_cmd sudo apt-get update -y
}

install_edge_intune_mde_packages() {
    # Ensure repos are present (safe/idempotent)
    install_microsoft_apt_repo

    run_cmd sudo apt-get update -y

    # Edge (from repos/edge)
    run_cmd sudo apt-get install -y microsoft-edge-stable

    # Intune portal (from microsoft ubuntu prod repo, if your tenant supports it)
    # If this package name ever changes, it will fail here (which is correct).
    run_cmd sudo apt-get install -y intune-portal

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install mdatp and run health check"
        return 0
    fi

    # Defender for Endpoint
    run_cmd sudo apt-get install -y mdatp

    if command -v mdatp >/dev/null 2>&1; then
        run_shell "mdatp health || true"
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
"Intune enrollment requires an interactive sign-in.\n\nNotes:\n- You can keep using Floorp daily.\n- Edge is installed ONLY to satisfy enrollment requirements.\n- Your WM (dwm) is fine AFTER enrollment.\n\nDo this once:\n1) Log into a session where GUI apps can be launched.\n2) Run: intune-portal\n3) Sign in with work account.\n4) Complete enrollment prompts.\n\nAfter enrollment:\n- Go back to dwm.\n- Leave Edge installed.\n\nLog:\n$LOG_FILE" \
22 104
}

# ============================================================
# Menu
# ============================================================

main_menu() {
    whiptail --title "Ubuntu .dotfiles Installer" --checklist "Select what to install:" 28 114 22 \
        "packages" "Install system packages (X + startx + build deps + firmware + microcode + docker + nvm)" ON \
        "stow" "Stow dotfiles into HOME (skips missing packages; backs up conflicts if needed)" ON \
        "wezterm" "Install WezTerm (apt.fury.io/wez)" ON \
        "neovim" "Install Neovim via PPA (neovim-ppa/unstable)" ON \
        "tailscale" "Install Tailscale (official script)" ON \
        "floorp" "Install Floorp browser (ppa.floorp.app)" ON \
        "floorp_profile" "Apply Floorp UI template (user.js + chrome/)" ON \
        "tampermonkey" "Show Tampermonkey import steps (if backup zip exists)" ON \
        "moonlight" "Install Moonlight (GitHub .deb; fallback to apt moonlight-qt)" ON \
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

    if grep -q "\"packages\"" <<<"$selected" || grep -q "\"nvidia_driver\"" <<<"$selected"; then
        hardware_prompt
    fi

    start_gauge
    ensure_dirs

    local ran=()

    if grep -q "\"packages\"" <<<"$selected"; then
        step 5 "Installing system packages (includes Docker + NVM)"
        install_base_packages
        ran+=("packages")
    fi

    if grep -q "\"stow\"" <<<"$selected"; then
        step 20 "Stowing dotfiles"
        stow_dotfiles
        ran+=("stow")
    fi

    if grep -q "\"wezterm\"" <<<"$selected"; then
        step 30 "Installing WezTerm"
        install_wezterm
        ran+=("wezterm")
    fi

    if grep -q "\"neovim\"" <<<"$selected"; then
        step 38 "Installing Neovim (PPA)"
        install_neovim_ppa
        ran+=("neovim")
    fi

    if grep -q "\"tailscale\"" <<<"$selected"; then
        step 46 "Installing Tailscale"
        install_tailscale
        ran+=("tailscale")
    fi

    if grep -q "\"floorp\"" <<<"$selected"; then
        step 54 "Installing Floorp"
        install_floorp
        ran+=("floorp")
    fi

    if grep -q "\"floorp_profile\"" <<<"$selected"; then
        step 58 "Applying Floorp UI template"
        floorp_apply_template
        ran+=("floorp_profile")
    fi

    if grep -q "\"moonlight\"" <<<"$selected"; then
        step 64 "Installing Moonlight"
        install_moonlight_latest
        ran+=("moonlight")
    fi

    if grep -q "\"sunshine\"" <<<"$selected"; then
        step 70 "Installing Sunshine"
        install_sunshine_latest
        sunshine_post_config
        ran+=("sunshine")
    fi

    if grep -q "\"steam\"" <<<"$selected"; then
        step 74 "Installing Steam"
        install_steam
        ran+=("steam")
    fi

    if grep -q "\"plex\"" <<<"$selected"; then
        step 78 "Installing Plex Media Server"
        install_plex_server
        ran+=("plex")
    fi

    if grep -q "\"suckless\"" <<<"$selected"; then
        step 84 "Initializing suckless submodules"
        ensure_suckless_submodules
        step 88 "Linking suckless repos into ~/repos"
        link_suckless_into_repos
        step 92 "Building and installing dwm/dmenu/slstatus"
        build_install_suckless
        ran+=("suckless")
    fi

    if grep -q "\"session\"" <<<"$selected"; then
        step 94 "Registering dwm session"
        register_dwm_session
        ran+=("session")
    fi

    if grep -q "\"fonts\"" <<<"$selected"; then
        step 96 "Installing fonts"
        install_nerd_font_firacode
        ran+=("fonts")
    fi

    if grep -q "\"shell\"" <<<"$selected"; then
        step 98 "Setting default shell"
        ensure_zsh_default_shell
        ran+=("shell")
    fi

    if grep -q "\"tampermonkey\"" <<<"$selected"; then
        step 99 "Tampermonkey import instructions"
        tampermonkey_prompt_import
        ran+=("tampermonkey")
    fi

    if grep -q "\"nvidia_driver\"" <<<"$selected"; then
        step 80 "Installing NVIDIA driver"
        install_nvidia_proprietary_driver
        ran+=("nvidia_driver")
    fi

    if grep -q "\"corp_enroll\"" <<<"$selected"; then
        step 45 "Installing Microsoft apt repo"
        install_microsoft_apt_repo
        step 60 "Installing Edge + Intune Portal + Defender"
        install_edge_intune_mde_packages
        step 70 "Defender onboarding (optional)"
        prompt_defender_onboarding
        step 75 "Showing Intune enrollment steps"
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

    local warn_text="none"
    if [[ "${#WARNINGS[@]}" -gt 0 ]]; then
        warn_text="$(printf "%s\n" "${WARNINGS[@]}" | sed 's/^/- /')"
    fi

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

    local docker_notes=""
    if grep -q "\"packages\"" <<<"$selected"; then
        docker_notes="
Docker Notes:
- Docker was installed from Docker's official repo.
- Your user was added to the docker group. You may need to log out/in for it to take effect.
- Verify: docker version"
    fi

    local nvm_notes=""
    if grep -q "\"packages\"" <<<"$selected"; then
        nvm_notes="
NVM Notes:
- NVM was installed to ~/.nvm (if not already present).
- Restart your shell, then verify: command -v nvm"
    fi

    whiptail --title "Installation Complete" --msgbox \
"$summary

Ran steps:
$ran_text

Warnings:
$warn_text
$hw_notes
$docker_notes
$nvm_notes
$corp_notes

Next steps:
- If you installed dwm session: Log out -> choose 'dwm' -> log in
- If Floorp template did not apply: launch Floorp once, close it, re-run with Floorp UI template checked
- If NVIDIA driver was installed: reboot

Log file:
$LOG_FILE" 30 104

    if whiptail --title "View Log" --yesno "Open the install log now?" 10 60; then
        whiptail --title "Install Log" --textbox "$LOG_FILE" 28 120
    fi

    log "=== dotfiles installer end ==="
}

# TODO:
#
# 1) Install latest node version
#
#   nvm install node
#   nvm use node
#
# 2) Install Mullvad VPN

main "$@"
