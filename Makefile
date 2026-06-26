SHELL := /usr/bin/env bash
DOTFILES_DIR := $(HOME)/.dotfiles
INSTALL := $(DOTFILES_DIR)/bootstrap/arch/install.sh

# Allow recipes to start with ">" instead of a TAB.
.RECIPEPREFIX := >

.PHONY: install dry-run tampermonkey-check tampermonkey-capture

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
