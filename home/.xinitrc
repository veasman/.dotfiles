# Compositor
xcompmgr -c -o 0 &

# Keybinds
sxhkd &

# Notifications
dunst &

# Hide idle cursor
unclutter -idle 1 -jitter 5 -root &

# Set wallpaper
$HOME/.local/bin/setbg &

# Set display resolutions
xrandr --output Virtual1 --primary --mode 1920x1080 -pos 0x0 --rotate normal --output Virtual2 --mode 1920x1080 --pos 1920x0 --rotate normal

# Run Emacs
exec dbus-launch --exit-with-session emacs -mm --debug-init
