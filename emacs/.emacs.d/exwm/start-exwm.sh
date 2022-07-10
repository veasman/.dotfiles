#!/bin/sh

sxhkd & disown
dunst & disown
picom & disown
unclutter & disown
exec dbus-launch --exit-with-session emacs -l ~/.emacs.d/init.el -mm --debug-init
