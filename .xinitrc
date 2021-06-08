xrdb -merge ~/.Xresources
sxhkd & # Hotkeys
dunst & # Notifications
picom & # Compositor
setbg & # Wallpaper
setres & # Resolution and Refresh Rate
unclutter -idle 1 & # Hide idle cursor
pulseaudio & # Audio
emacs --daemon & # Emacs daemon for loading config
exec dbus-launch --sh-syntax --exit-with-session ~/.local/bin/scripts/startdwm # Window Manager
