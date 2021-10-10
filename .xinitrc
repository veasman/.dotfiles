xrdb -merge ~/.config/xorg/Xresources
sxhkd & # Hotkeys
dunst & # Notifications
picom & # Compositor
setbg & # Wallpaper
xrandr -r 120 # Refresh rate
unclutter -idle 2 & # Hide idle cursor
pipewire-start.sh # Audio
exec dbus-launch --sh-syntax --exit-with-session dwm
