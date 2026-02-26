# xinit-desktop/.xinitrc
#!/bin/sh

[ -f "$HOME/.config/xorg/Xresources" ] && xrdb -merge "$HOME/.config/xorg/Xresources"

sxhkd &
dunst &
picom &
setbg &
xrandr -r 120
unclutter -idle 2 &

slstatus &

exec dwm