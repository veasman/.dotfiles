# Artix Dotfiles

Artix Linux (OpenRC) dotfiles using:

- vwm
- rofi
- stow-managed configs

---

## Install

### Phase 1 — Inside `artix-chroot`

```bash
curl -fsSL https://raw.githubusercontent.com/veasman/.dotfiles/master/bootstrap/arch/pre-install.sh | bash
```

Creates user, generates SSH key, clones dotfiles.

### Phase 2 — After reboot and login

```bash
~/.dotfiles/bootstrap/arch/install.sh
```

Or:

```bash
cd ~/.dotfiles
make install
```

Dry run:

```bash
make dry-run
```
