#!/usr/bin/env sh
# Restart WiFi drivers and NetworkManager — init-agnostic.
# Use doas if available, fall back to sudo.

SUDO=""
command -v doas &>/dev/null && SUDO="doas" || SUDO="sudo"

if command -v systemctl &>/dev/null; then
    $SUDO systemctl stop iwd 2>/dev/null || true
    $SUDO systemctl restart NetworkManager
else
    $SUDO rc-service iwd stop 2>/dev/null || true
    $SUDO rc-service NetworkManager restart
fi

$SUDO ip link set wlan0 up 2>/dev/null || true
$SUDO ip link set wlan1 up 2>/dev/null || true
$SUDO rfkill unblock all
$SUDO modprobe -r mt7921e 2>/dev/null && $SUDO modprobe mt7921e
