#!/usr/bin/env sh
xrandr --setprovideroutputsource 1 0 || true
xrandr --setprovideroutputsource 2 0 || true
xrandr \
  --output eDP-1 --off \
  --output DVI-I-1-1 --mode 1920x1080 --rotate left --pos 0x0 \
  --output DP-2 --mode 1920x1080 --rotate normal --primary --pos 1080x0 \
  --output DVI-I-2-2 --mode 1920x1080 --rotate normal --pos 3000x0
