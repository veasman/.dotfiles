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

require_ubuntu() {
    [[ -r /etc/os-release ]] || die_ui "/etc/os-release not found"

    # shellcheck disable=SC1091
    . /etc/os-release

    if [[ "${ID:-}" != "ubuntu" && "${ID_LIKE:-}" != *"ubuntu"* ]]; then
        die_ui "This installer is for Ubuntu.\n\nYou said you want Arch support later. Fine. This is not that script."
    fi

    if [[ "${VERSION_ID:-}" != "24.04" ]]; then
        warn "Tested target is Ubuntu 24.04, current VERSION_ID=${VERSION_ID:-unknown}"
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
    sudo apt-get update -y >>"$LOG_FILE" 2>&1
    sudo apt-get install -y whiptail >>"$LOG_FILE" 2>&1
}

start_gauge() {
    exec 4> >(whiptail --title "Ubuntu dotfiles bootstrap" \
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

    env \
        DEBIAN_FRONTEND=noninteractive \
        NEEDRESTART_MODE=a \
        APT_LISTCHANGES_FRONTEND=none \
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

    env \
        DEBIAN_FRONTEND=noninteractive \
        NEEDRESTART_MODE=a \
        APT_LISTCHANGES_FRONTEND=none \
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
        "sunshine" "Install Sunshine" OFF \
        "steam" "Install Steam" OFF \
        "mullvad" "Install Mullvad VPN" OFF \
        3>&1 1>&2 2>&3
    )" || die_ui "Aborted"

    INSTALL_SUNSHINE=0
    INSTALL_STEAM=0
    INSTALL_MULLVAD=0

    if [[ "$result" == *'"sunshine"'* ]]; then
        INSTALL_SUNSHINE=1
    fi

    if [[ "$result" == *'"steam"'* ]]; then
        INSTALL_STEAM=1
    fi

    if [[ "$result" == *'"mullvad"'* ]]; then
        INSTALL_MULLVAD=1
    fi

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

apt_install() {
    run_cmd sudo apt-get install -y "$@"
}

apt_update_upgrade() {
    run_cmd sudo apt-get update -y
    #run_cmd sudo apt-get upgrade -y
}

install_cpu_microcode() {
    local vendor=""

    if grep -qi "AuthenticAMD" /proc/cpuinfo 2>/dev/null; then
        vendor="amd"
    elif grep -qi "GenuineIntel" /proc/cpuinfo 2>/dev/null; then
        vendor="intel"
    else
        vendor="unknown"
    fi

    case "$vendor" in
        amd)
            log "[cpu] detected AMD CPU"
            run_cmd sudo apt-get install -y amd64-microcode
            ;;
        intel)
            log "[cpu] detected Intel CPU"
            run_cmd sudo apt-get install -y intel-microcode
            ;;
        *)
            warn "Could not determine CPU vendor for microcode install; skipping"
            ;;
    esac
}

install_base_packages() {
    step 5 "Refreshing apt metadata"
    run_cmd sudo apt-get update -y

    step 8 "Installing bootstrap prerequisites"
    run_cmd sudo apt-get install -y \
        ca-certificates curl gnupg software-properties-common apt-transport-https whiptail

    step 12 "Installing core development packages"
    run_cmd sudo apt-get install -y \
        git stow build-essential pkg-config gcc make \
        python3 python3-pip unzip wget fontconfig

    step 18 "Installing shell and terminal tools"
    run_cmd sudo apt-get install -y \
        zsh tmux fzf tree ripgrep fd-find xclip

    step 24 "Installing X11 desktop utilities"
    run_cmd sudo apt-get install -y \
        xinit x11-xserver-utils xserver-xorg-core dbus-x11 \
        rofi dunst libnotify-bin picom xwallpaper playerctl flameshot brightnessctl

    step 30 "Installing VWM build dependencies"
    run_cmd sudo apt-get install -y \
        libx11-dev libx11-xcb-dev libxcb1-dev libxcb-randr0-dev \
        libxcb-icccm4-dev libxcb-keysyms1-dev libxft-dev \
        libfontconfig1-dev libcairo2-dev libxrender-dev libxext-dev

    step 36 "Installing system utilities"
    run_cmd sudo apt-get install -y \
        linux-firmware pciutils usbutils lm-sensors rfkill \
        mesa-utils vulkan-tools libvulkan1 mesa-vulkan-drivers

    step 42 "Installing LaTeX tooling"
    run_cmd sudo apt-get install -y \
        texlive-latex-base texlive-latex-extra latexmk zathura zathura-pdf-poppler

    install_cpu_microcode
}

install_floorp() {
    run_cmd sudo mkdir -p /usr/share/keyrings
    run_cmd sudo mkdir -p /etc/apt/sources.list.d

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] configure Floorp repo and install floorp"
        return 0
    fi

    run_shell '
        tmp_key="$(mktemp)"
        tmp_list="$(mktemp)"

        curl -fsSL https://ppa.floorp.app/KEY.gpg -o "$tmp_key"
        sudo rm -f /usr/share/keyrings/Floorp.gpg
        sudo gpg --dearmor --yes -o /usr/share/keyrings/Floorp.gpg "$tmp_key"

        curl -fsSL https://ppa.floorp.app/Floorp.list -o "$tmp_list"
        sudo install -m 0644 "$tmp_list" /etc/apt/sources.list.d/Floorp.list

        rm -f "$tmp_key" "$tmp_list"
    '

    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y floorp
}

install_tailscale() {
    run_cmd sudo mkdir -p /usr/share/keyrings
    run_cmd sudo mkdir -p /etc/apt/sources.list.d

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Tailscale from apt repository"
        return 0
    fi

    run_shell '
        tmp_key="$(mktemp)"
        curl -fsSL https://pkgs.tailscale.com/stable/ubuntu/noble.noarmor.gpg -o "$tmp_key"
        sudo install -m 0644 "$tmp_key" /usr/share/keyrings/tailscale-archive-keyring.gpg
        rm -f "$tmp_key"

        cat <<EOF | sudo tee /etc/apt/sources.list.d/tailscale.list >/dev/null
deb [signed-by=/usr/share/keyrings/tailscale-archive-keyring.gpg] https://pkgs.tailscale.com/stable/ubuntu noble main
EOF
    '

    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y tailscale
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
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install Mullvad VPN via official repository"
        return 0
    fi

    run_shell 'curl -fsSLo /tmp/mullvad-repo.sh https://repository.mullvad.net/deb/mullvad-keyring.sh && sudo sh /tmp/mullvad-repo.sh && rm -f /tmp/mullvad-repo.sh'
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y mullvad-vpn
}

install_docker() {
    run_cmd sudo apt-get update -y
    apt_install ca-certificates curl gnupg

    run_cmd sudo install -d -m 0755 /etc/apt/keyrings

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] configure Docker apt repo and install Docker"
        return 0
    fi

    run_shell 'curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor --yes -o /etc/apt/keyrings/docker.gpg'
    run_cmd sudo chmod a+r /etc/apt/keyrings/docker.gpg

    # shellcheck disable=SC1091
    . /etc/os-release
    local codename="${UBUNTU_CODENAME:-${VERSION_CODENAME:-noble}}"

    sudo tee /etc/apt/sources.list.d/docker.list >/dev/null <<EOF
deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu ${codename} stable
EOF

    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y \
        docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

    run_cmd sudo systemctl enable --now docker
    run_cmd sudo usermod -aG docker "$USER"
}

install_nvm_and_node() {
    if [[ ! -d "$HOME/.nvm" ]]; then
        run_shell 'curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.4/install.sh | bash'
    fi

    run_shell 'nvm install --lts'
    run_shell 'nvm use --lts'
}

install_neovim_nightly() {
    run_cmd mkdir -p "$HOME/.local/bin"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] download Neovim nightly tarball and install to ~/.local/bin/nvim"
        return 0
    fi

    run_shell '
        tmpdir="$(mktemp -d)"
        cd "$tmpdir"

        curl -fL -o nvim-linux-x86_64.tar.gz \
            https://github.com/neovim/neovim/releases/download/nightly/nvim-linux-x86_64.tar.gz

        tar -xzf nvim-linux-x86_64.tar.gz

        install -Dm755 nvim-linux-x86_64/bin/nvim "$HOME/.local/bin/nvim"

        rm -rf "$tmpdir"
    '
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
    run_shell "cd '$VWM_DIR' && make && make install"
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
        local rel
        rel="${src#"$package_dir"/}"

        if [[ -z "$rel" ]]; then
            continue
        fi

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
        fuzzel
        git
        kitty
        latex
        mako
        nvim
        pmux
        rofi
        scripts
        shell
        sway
        tmux
        vwm
        waybar
        xprofile-desktop
        xresources
    )

    for pkg in "${packages[@]}"; do
        stow_package_force "$pkg"
    done

    run_cmd fc-cache -f
}

generate_vwm_session_file() {
    local vwm_bin
    vwm_bin="$(command -v vwm || true)"
    [[ -n "$vwm_bin" ]] || die_ui "vwm binary not found after install"

    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] write /usr/share/xsessions/vwm.desktop"
        return 0
    fi

    sudo tee /usr/share/xsessions/vwm.desktop >/dev/null <<EOF
[Desktop Entry]
Name=vwm
Comment=veasman's window manager
Exec=$vwm_bin
Type=Application
DesktopNames=vwm
EOF
}

ensure_zsh_default_shell() {
    local user="${SUDO_USER:-$USER}"
    local target_shell="/bin/zsh"

    [[ -x "$target_shell" ]] || die_ui "$target_shell not found"

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
    run_cmd sudo dpkg --add-architecture i386
    run_cmd sudo apt-get update -y
    run_cmd sudo apt-get install -y steam
}

github_latest_asset_url() {
    local owner="$1"
    local repo="$2"
    local regex="$3"

    python3 - "$owner" "$repo" "$regex" <<'PY'
import json
import re
import sys
import urllib.request

owner, repo, regex = sys.argv[1:]
url = f"https://api.github.com/repos/{owner}/{repo}/releases/latest"
req = urllib.request.Request(url, headers={"User-Agent": "dotfiles-bootstrap"})
with urllib.request.urlopen(req) as r:
    data = json.load(r)

pat = re.compile(regex)
for asset in data.get("assets", []):
    name = asset.get("name", "")
    dl = asset.get("browser_download_url", "")
    if dl and (pat.search(name) or pat.search(dl)):
        print(dl)
        sys.exit(0)

sys.exit(1)
PY
}

install_sunshine() {
    if [[ "$DRY_RUN" -eq 1 ]]; then
        log "[DRY RUN] install latest Sunshine .deb"
        return 0
    fi

    local url
    url="$(github_latest_asset_url "LizardByte" "Sunshine" '(?i)(ubuntu|debian|linux).*(amd64|x86_64).*\.deb$')" \
        || die_ui "Could not resolve Sunshine .deb URL"

    local tmpdir
    tmpdir="$(mktemp -d)"
    run_shell "cd '$tmpdir' && curl -fLO '$url' && sudo dpkg -i ./*.deb || sudo apt-get -f install -y"
    run_cmd sudo apt-get -f install -y
    run_cmd rm -rf "$tmpdir"

    run_cmd sudo systemctl enable sunshine || true
}

tampermonkey_reminder() {
    local zip="$DOTFILES_DIR/assets/tampermonkey/tampermonkey-backup.zip"
    [[ -f "$zip" ]] || return 0

    whiptail --title "Tampermonkey reminder" --msgbox \
"Tampermonkey backup detected:\n$zip\n\nImport it manually inside Floorp:\n1) Open Floorp\n2) Open Tampermonkey dashboard\n3) Utilities -> Import\n4) Select the zip file" 18 100
}

post_install_notes() {
    local notes=""
    local warns="none"

    if [[ "${#WARNINGS[@]}" -gt 0 ]]; then
        warns="$(printf '%s\n' "${WARNINGS[@]}" | sed 's/^/- /')"
    fi

    notes+="Completed.

Warnings:
$warns

Next steps:
- Log out and choose the 'vwm' session in GDM
- Run 'docker run hello-world' after re-login to confirm Docker group access
- If Tailscale is not connected yet, run: sudo tailscale up
- Open Floorp and manually import the Tampermonkey backup if you want it
- Reboot if anything graphics/session-related acts stupid
"

    if [[ "$INSTALL_SUNSHINE" -eq 1 ]]; then
        notes+="
Sunshine:
- Open Sunshine from the app menu or service setup
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
    log "=== bootstrap start (dry_run=$DRY_RUN) ==="

    require_ubuntu
    ensure_sudo
    ensure_whiptail
    ensure_dirs
    optional_prompt

    [[ -d "$DOTFILES_DIR" ]] || die_ui "Dotfiles directory not found: $DOTFILES_DIR"

    start_gauge

    step 5 "Installing Ubuntu packages"
    install_base_packages

    step 20 "Installing Floorp"
    install_floorp

    step 30 "Installing Tailscale"
    install_tailscale

    step 38 "Installing Docker"
    install_docker

    step 46 "Installing NVM, Node LTS, treesitter-cli"
    install_nvm_and_node

    step 54 "Installing Neovim nightly"
    install_neovim_nightly

    step 62 "Installing vwm repo"
    install_vwm

    step 70 "Installing loom repo"
    install_loom

    step 76 "Installing pmux repo"
    install_pmux

    step 84 "Stowing dotfiles with forced replacement"
    stow_dotfiles

    step 90 "Generating GDM session for vwm"
    generate_vwm_session_file

    step 94 "Setting default shell to zsh"
    ensure_zsh_default_shell

    if [[ "$INSTALL_SUNSHINE" -eq 1 ]]; then
        step 96 "Installing Sunshine"
        install_sunshine
    fi

    if [[ "$INSTALL_STEAM" -eq 1 ]]; then
        step 97 "Installing Steam"
        install_steam
    fi

    if [[ "$INSTALL_MULLVAD" -eq 1 ]]; then
        step 98 "Installing Mullvad"
        install_mullvad
    fi

    step 99 "Applying default loom theme"
    #apply_default_theme

    finish_gauge

    tailscale_post
    tampermonkey_reminder
    post_install_notes

    log "=== bootstrap end ==="
}

main "$@"
