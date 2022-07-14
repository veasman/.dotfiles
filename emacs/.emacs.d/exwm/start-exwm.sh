#!/bin/sh

sxhkd &
dunst &
picom &
unclutter &
setbg &
exec dbus-launch --exit-with-session emacs -l ~/.emacs.d/init.el -mm --debug-init
