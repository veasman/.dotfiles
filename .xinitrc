xrdb -merge ~/.config/xorg/Xresources

xrandr -s 1920x1080 -r 144

~/.local/bin/setbg & # Wallpaper
~/.local/bin/setmouseaccel & # Disable mouse accel

sxhkd & # Hotkeys
dunst & # Notifications
picom & # Compositor
unclutter -idle 2 & # Hide idle cursor
pulseaudio & # Audio

exec dwm
