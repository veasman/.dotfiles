#!/bin/sh

sxhkd &
dunst &
picom &
unclutter &
setbg &
exec dbus-launch --exit-with-session emacs -mm --debug-init
