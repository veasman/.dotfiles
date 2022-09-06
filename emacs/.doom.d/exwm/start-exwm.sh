#!/bin/sh
exec dbus-launch --exit-with-session emacs -mm --debug-init --with-x-toolkit=lucid
