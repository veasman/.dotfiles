#!/usr/bin/env sh

[ -f "$HOME/.config/xorg/Xresources" ] && xrdb -merge "$HOME/.config/xorg/Xresources"

xsetroot -cursor_name left_ptr

xinput | grep "BT mouse" && xinput --set-prop "BT mouse" "libinput Accel Profile Enabled" 0, 1
xinput | grep "BT mouse" && xinput --set-prop "BT mouse" "libinput Accel Speed" 0

exec dbus-run-session -- sh -lc '
    [ -f "${XDG_STATE_HOME:-$HOME/.local/state}/loom/generated/session-theme.sh" ] && . "${XDG_STATE_HOME:-$HOME/.local/state}/loom/generated/session-theme.sh"

    if [ -f "${XDG_STATE_HOME:-$HOME/.local/state}/loom/current_wallpaper" ]; then
        BG_PATH="$(cat "${XDG_STATE_HOME:-$HOME/.local/state}/loom/current_wallpaper")"
        if [ -f "$BG_PATH" ]; then
            mkdir -p "${XDG_DATA_HOME:-$HOME/.local/share}"
            ln -snf "$BG_PATH" "${XDG_DATA_HOME:-$HOME/.local/share}/bg"
            xwallpaper --zoom "$BG_PATH"
        fi
    fi

    xrandr --setprovideroutputsource 1 0 || true

    exec vwm
'
