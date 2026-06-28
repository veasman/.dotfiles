SHELL := /usr/bin/env bash
DOTFILES_DIR := $(HOME)/.dotfiles
INSTALL := $(DOTFILES_DIR)/bootstrap/arch/install.sh

# Allow recipes to start with ">" instead of a TAB.
.RECIPEPREFIX := >

.PHONY: install dry-run tampermonkey-check tampermonkey-capture guide

install:
>   $(INSTALL)

dry-run:
>   $(INSTALL) --dry-run

tampermonkey-check:
>   test -f "$(DOTFILES_DIR)/assets/tampermonkey/tampermonkey-backup.zip" && \
>       echo "OK: assets/tampermonkey/tampermonkey-backup.zip exists" || \
>       (echo "MISSING: assets/tampermonkey/tampermonkey-backup.zip" && exit 1)

tampermonkey-capture:
>   mkdir -p "$(DOTFILES_DIR)/assets/tampermonkey"
>   newest="$$(ls -t "$(HOME)"/Downloads/*tampermonkey*.zip 2>/dev/null | head -n 1)"; \
>   if [[ -z "$$newest" ]]; then \
>       echo "No Tampermonkey zip found in $(HOME)/Downloads. Export it from Tampermonkey -> Utilities -> Export first."; \
>       exit 1; \
>   fi; \
>   cp -f "$$newest" "$(DOTFILES_DIR)/assets/tampermonkey/tampermonkey-backup.zip"; \
>   echo "Captured: $$newest -> assets/tampermonkey/tampermonkey-backup.zip"

# ────────────────────────────────
# post-install guidance
# ────────────────────────────────

guide:
>   @printf '\n'
>   @printf '  \033[1mPost-install next steps\033[0m\n'
>   @printf '  ────────────────────────────────────────────────────────────\n'
>   @printf '\n'
>   @printf '  \033[1mLaunch Hyprland\033[0m\n'
>   @printf '    start-hyprland              # from a TTY (no sudo)\n'
>   @printf '    Or add to ~/.zprofile for auto-start on tty1:\n'
>   @printf '      if [ "$$(tty)" = "/dev/tty1" ]; then\n'
>   @printf '        exec start-hyprland\n'
>   @printf '      fi\n'
>   @printf '\n'
>   @printf '  \033[1mLockscreen\033[0m (hyprlock — already configured)\n'
>   @printf '    Super+Shift+Space          # lock manually\n'
>   @printf '    Auto-locks after 10min idle (hypridle)\n'
>   @printf '    Config: ~/.config/hypr/hyprlock.conf\n'
>   @printf '\n'
>   @printf '  \033[1mIdle / screensaver\033[0m (hypridle — already configured)\n'
>   @printf '    10min → lock\n'
>   @printf '    15min → DPMS off (monitor sleep)\n'
>   @printf '    30min → system suspend\n'
>   @printf '    Config: ~/.config/hypr/hypridle.conf\n'
>   @printf '\n'
>   @printf '  \033[1mGRUB theme\033[0m (not managed by dotfiles)\n'
>   @printf '    Theme dir: /boot/grub/themes/\n'
>   @printf '    Vimix theme (recommended):\n'
>   @printf '      paru -S grub-theme-vimix\n'
>   @printf '      # then edit /etc/default/grub:\n'
>   @printf '      #   GRUB_THEME="/boot/grub/themes/Vimix/theme.txt"\n'
>   @printf '      sudo grub-mkconfig -o /boot/grub/grub.cfg\n'
>   @printf '\n'
>   @printf '  \033[1mHermes\033[0m — LLM agent setup\n'
>   @printf '    Edit ~/.hermes/.env       # add OPENROUTER_API_KEY etc.\n'
>   @printf '    hermes model --list       # verify connection\n'
>   @printf '    hermes-setup --all        # enable cron/gateway\n'
>   @printf '\n'
>   @printf '  \033[1mDocker\033[0m\n'
>   @printf '    sudo usermod -aG docker $$USER && newgrp docker\n'
>   @printf '    docker run hello-world    # confirm it works\n'
>   @printf '\n'
>   @printf '  \033[1mTailscale\033[0m\n'
>   @printf '    sudo tailscale up         # authenticate\n'
>   @printf '\n'
>   @printf '  \033[1mFreeLLMAPI\033[0m\n'
>   @printf '    Already running at localhost:3001\n'
>   @printf '    API key synced to opencode/hermes configs\n'
>   @printf '\n'
>   @printf '  \033[1mFloorp\033[0m\n'
>   @printf '    Import Tampermonkey backup manually if desired\n'
>   @printf '\n'
>   @printf '  \033[1mSunshine\033[0m\n'
>   @printf '    Launch Sunshine — complete host setup & pairing\n'
>   @printf '\n'
>   @printf '  \033[1mReboot if graphics/input acts up\033[0m\n'
>   @printf '\n'

HERMES_ENV := $(HOME)/.hermes/.env
HERMES_ENV_TEMPLATE := $(DOTFILES_DIR)/hermes/.env.template
HERMES_CONFIG := $(HOME)/.hermes/config.yaml
HERMES_CONFIG_EXAMPLE := $(DOTFILES_DIR)/hermes/config.yaml.example

.PHONY: hermes-env hermes-config

hermes-env:
>   @if [ -f "$(HERMES_ENV)" ]; then \
>       echo "OK: $(HERMES_ENV) already exists"; \
>   else \
>       echo "Creating $(HERMES_ENV) from template..."; \
>       cp "$(HERMES_ENV_TEMPLATE)" "$(HERMES_ENV)"; \
>       chmod 600 "$(HERMES_ENV)"; \
>       echo ""; \
>       echo "IMPORTANT: Edit $(HERMES_ENV) and set your API keys:"; \
>       echo "  OPENROUTER_API_KEY — required (all LLM routes through OpenRouter)"; \
>       echo "  BRAVE_SEARCH_API_KEY — required for web search"; \
>       echo "  FREEFLLMAPI_API_KEY — required for free-tier (Kimi K2.6)"; \
>       echo ""; \
>       echo "Then verify with: hermes model"; \
>   fi

hermes-config:
>   @if [ -f "$(HERMES_CONFIG)" ]; then \
>       echo "OK: $(HERMES_CONFIG) already exists"; \
>   else \
>       echo "Creating $(HERMES_CONFIG) from example..."; \
>       cp "$(HERMES_CONFIG_EXAMPLE)" "$(HERMES_CONFIG)"; \
>       chmod 644 "$(HERMES_CONFIG)"; \
>       echo "Done."; \
>   fi
