#!/usr/bin/env bash
set -eu

INTERNAL="eDP-1"
sleep 1

MONS="$(hyprctl monitors all)"
ACTIVE_EXTERNALS="$(printf '%s\n' "$MONS" | grep '^Monitor ' | grep -vc "$INTERNAL" || true)"

if [ "$ACTIVE_EXTERNALS" -gt 0 ]; then
    hyprctl keyword monitor "$INTERNAL,disable"
else
    hyprctl keyword monitor "$INTERNAL,preferred,auto,1"
fi
