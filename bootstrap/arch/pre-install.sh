#!/usr/bin/env bash
# Artix OpenRC pre-install — run inside artix-chroot as root.
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/veasman/.dotfiles/master/bootstrap/arch/pre-install.sh | bash
#
# This script:
#   1. Installs minimal prerequisites (git, openssh, sudo)
#   2. Creates a regular user with wheel/sudo access
#   3. Generates an SSH key and waits for you to add it to GitHub
#   4. Clones the dotfiles repo as that user
#
# After this script finishes, complete the rest of the base Artix setup
# (bootloader, locale, timezone, etc.), reboot, log in, then run:
#   ~/.dotfiles/bootstrap/arch/install.sh

set -Eeuo pipefail

DOTFILES_REPO="git@github.com:veasman/.dotfiles.git"

bold()  { printf '\033[1m%s\033[0m\n' "$*"; }
green() { printf '\033[1;32m%s\033[0m\n' "$*"; }
red()   { printf '\033[1;31m%s\033[0m\n' "$*"; }
step()  { printf '\n\033[1;36m=> %s\033[0m\n' "$*"; }

die() { red "ERROR: $*"; exit 1; }

[[ "$(id -u)" -eq 0 ]] || die "This script must be run as root (inside artix-chroot)."

# -------------------------------------------------------------------
# 1. Install minimal packages
# -------------------------------------------------------------------
step "Installing prerequisites"
pacman -Sy --needed --noconfirm base-devel git openssh curl sudo

# -------------------------------------------------------------------
# 2. Configure sudoers for wheel group
# -------------------------------------------------------------------
step "Configuring sudo for wheel group"
if ! grep -qE '^\s*%wheel\s+ALL=\(ALL(:ALL)?\)\s+ALL' /etc/sudoers; then
    sed -i 's/^#\s*\(%wheel ALL=(ALL:ALL) ALL\)/\1/' /etc/sudoers
    if ! grep -qE '^\s*%wheel\s+ALL=\(ALL(:ALL)?\)\s+ALL' /etc/sudoers; then
        echo '%wheel ALL=(ALL:ALL) ALL' >> /etc/sudoers
    fi
fi
bold "wheel group has sudo access"

# -------------------------------------------------------------------
# 3. Create user
# -------------------------------------------------------------------
step "User setup"
read -rp "Username [oracle]: " USERNAME
USERNAME="${USERNAME:-oracle}"

if id "$USERNAME" &>/dev/null; then
    bold "User '$USERNAME' already exists — skipping creation"
else
    useradd -m -G wheel -s /bin/bash "$USERNAME"
    bold "Created user '$USERNAME'"
    bold "Set a password:"
    passwd "$USERNAME"
fi

USER_HOME="$(eval echo "~$USERNAME")"

# -------------------------------------------------------------------
# 4. Generate SSH key
# -------------------------------------------------------------------
step "SSH key setup"
SSH_KEY="$USER_HOME/.ssh/id_ed25519"

if [[ -f "$SSH_KEY" ]]; then
    bold "SSH key already exists at $SSH_KEY"
else
    su - "$USERNAME" -c "ssh-keygen -t ed25519 -C '$USERNAME@artix' -f '$SSH_KEY' -N ''"
fi

echo ""
green "=== Public key ==="
cat "${SSH_KEY}.pub"
green "==================="
echo ""
bold "Add this key to GitHub: https://github.com/settings/ssh/new"
echo ""

# -------------------------------------------------------------------
# 5. Wait for GitHub SSH confirmation
# -------------------------------------------------------------------
while true; do
    read -rp "Press Enter after adding the key to GitHub (or 'q' to skip)... "
    [[ "$REPLY" == "q" ]] && break

    if su - "$USERNAME" -c "ssh -o StrictHostKeyChecking=accept-new -T git@github.com" 2>&1 | grep -qi "success\|authenticated"; then
        green "SSH to GitHub confirmed!"
        break
    else
        red "SSH test failed — make sure the key is added to GitHub and try again."
    fi
done

# -------------------------------------------------------------------
# 6. Clone dotfiles
# -------------------------------------------------------------------
step "Cloning dotfiles"
DOTFILES_TARGET="$USER_HOME/.dotfiles"

if [[ -d "$DOTFILES_TARGET/.git" ]]; then
    bold "Dotfiles already cloned at $DOTFILES_TARGET"
else
    su - "$USERNAME" -c "git clone '$DOTFILES_REPO' '$DOTFILES_TARGET'"
    green "Cloned to $DOTFILES_TARGET"
fi

# -------------------------------------------------------------------
# Done
# -------------------------------------------------------------------
echo ""
green "================================================"
green "  Pre-install complete!"
green "================================================"
echo ""
bold "Next steps:"
echo "  1. Finish base Artix setup (bootloader, locale, timezone, etc.)"
echo "  2. Reboot into the installed system"
echo "  3. Log in as '$USERNAME'"
echo "  4. Run: ~/.dotfiles/bootstrap/arch/install.sh"
echo ""
