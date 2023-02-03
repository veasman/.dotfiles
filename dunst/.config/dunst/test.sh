#!/usr/bin/env sh

pkill dunst
dunst &
notify-send "Notifcation Title" "Hello, World!"
