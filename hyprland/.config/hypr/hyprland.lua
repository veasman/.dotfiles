-- hyprland.lua — ~/.config/hypr/hyprland.lua
--
-- Migrated from hyprland.conf for Hyprland 0.57 (hyprlang removed).
-- Reload: mod+Shift+r. Exit: mod+Shift+q.

local home = os.getenv("HOME")

-- ─── Programs ────────────────────────────────────────────────────────
local mod         = "SUPER"
local terminal    = "hyprland-terminal"
local browser     = "floorp"
local fileManager = "hyprland-terminal lf"
local launcher    = "fuzzel --config=$HOME/.config/fuzzel/fuzzel-hyprland.ini"

-- ─── Monitors ────────────────────────────────────────────────────────
hl.monitor({ output = "", mode = "preferred", position = "auto", scale = 1 })
hl.monitor({ output = "DP-1", mode = "1920x1080@120", position = "1920x0", scale = 1 })
hl.monitor({ output = "eDP-1", mode = "preferred", position = "auto", scale = 1 })

-- Host-specific overrides (see hyprland-host.conf)
hl.monitor({ output = "HDMI-A-1", mode = "1920x1080@60", position = "auto", scale = 1 })

-- ─── Workspaces ──────────────────────────────────────────────────────
hl.workspace_rule({ workspace = "r[1-9]" })
hl.workspace_rule({ workspace = "r[11-19]" })
hl.workspace_rule({ workspace = "r[21-29]" })
hl.workspace_rule({ workspace = "r[31-39]" })

-- ─── General / decoration ────────────────────────────────────────────
hl.config({
    general = {
        border_size = 1,
        gaps_in = 4,
        gaps_out = 8,
        col = {
            active_border = {
                colors = { "rgb(fe8019)", "rgb(d3869b)", "rgb(d79921)", "rgb(fe8019)" },
                angle = 45,
            },
            inactive_border = "rgb(2a2a2a)",
        },
        layout = "dwindle",
        resize_on_border = true,
        no_focus_fallback = true,
        allow_tearing = false,
    },
    decoration = {
        rounding = 10,
        dim_inactive = true,
        dim_strength = 0.06,
        dim_special = 0.45,
        active_opacity = 1.0,
        inactive_opacity = 1.0,
        blur = {
            enabled = true,
            size = 4,
            passes = 1,
            new_optimizations = true,
            ignore_opacity = false,
        },
        shadow = {
            enabled = true,
            range = 8,
            render_power = 2,
            offset = { 0, 0 },
            color = "rgba(fe80193a)",
            color_inactive = "rgba(00000038)",
        },
    },
    animations = {
        enabled = true,
    },
    dwindle = {
        preserve_split = true,
        force_split = 0,
    },
    input = {
        kb_layout = "us",
        follow_mouse = 0,
        mouse_refocus = false,
        sensitivity = 0,
        accel_profile = "flat",
        touchpad = {
            natural_scroll = false,
            tap_to_click = true,
            disable_while_typing = true,
        },
    },
    debug = {
        vfr = true,
    },
    misc = {
        font_family = "Sans, FiraCode Nerd Font",
        disable_hyprland_logo = true,
        disable_splash_rendering = true,
        focus_on_activate = false,
        force_default_wallpaper = 0,
        background_color = "rgb(1d2021)",
        enable_swallow = false,
        on_focus_under_fullscreen = 1,
        exit_window_retains_fullscreen = true,
    },
    cursor = {
        inactive_timeout = 2,
        no_warps = false,
        warp_on_change_workspace = true,
    },
})

-- ─── Animations ──────────────────────────────────────────────────────
hl.curve("snap",  { type = "bezier", points = { { 0.22, 1 }, { 0.36, 1 } } })
hl.curve("pop",   { type = "bezier", points = { { 0.34, 1.56 }, { 0.64, 1 } } })
hl.curve("liner", { type = "bezier", points = { { 1, 1 }, { 1, 1 } } })

hl.animation({ leaf = "windows",          enabled = true,  speed = 2,  bezier = "snap" })
hl.animation({ leaf = "windowsOut",       enabled = true,  speed = 2,  bezier = "snap", style = "popin 80%" })
hl.animation({ leaf = "workspaces",       enabled = false })
hl.animation({ leaf = "specialWorkspace", enabled = true,  speed = 3,  bezier = "snap", style = "slidefadevert" })
hl.animation({ leaf = "fade",             enabled = true,  speed = 2,  bezier = "snap" })
hl.animation({ leaf = "border",           enabled = true,  speed = 2,  bezier = "snap" })
hl.animation({ leaf = "borderangle",      enabled = true,  speed = 40, bezier = "liner", style = "loop" })

-- ─── Environment ─────────────────────────────────────────────────────
hl.env("XCURSOR_THEME", "Banana")
hl.env("XCURSOR_SIZE", "34")
hl.env("HYPRCURSOR_THEME", "Banana")
hl.env("HYPRCURSOR_SIZE", "20")
hl.env("PATH", home .. "/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/bin")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")
hl.env("XDG_SESSION_TYPE", "wayland")
hl.env("QT_QPA_PLATFORM", "wayland")
hl.env("MOZ_ENABLE_WAYLAND", "1")
hl.env("_JAVA_AWT_WM_NONREPARENTING", "1")
hl.env("ELECTRON_OZONE_PLATFORM_HINT", "wayland")
hl.env("GTK_THEME", "Adwaita:dark")

hl.env("DXVK_HDR", "1")
hl.env("GAMESCOPE_HDR_ENABLED", "1")
hl.env("ENABLE_HDR_WSI", "1")

-- ─── Window / layer rules ────────────────────────────────────────────
hl.window_rule({ match = { class = "^(pavucontrol)$" }, float = true, size = "60% 60%", center = true })
hl.window_rule({ match = { class = "^(mpv)$" },         float = true, size = "80% 80%", center = true })
hl.window_rule({ match = { class = "^(lf-portal)$" },   float = true, size = "75% 80%", center = true })
hl.window_rule({ match = { title = "^(Open File|Save As)$" }, float = true })

hl.window_rule({ match = { class = ".*" }, no_blur = true })

hl.layer_rule({ match = { namespace = "waybar" }, blur = true })

-- Scratchpads
hl.window_rule({
    match = { class = "^(scratch-todo|scratch-glances)$" },
    workspace = "special:todo silent",
})
hl.window_rule({
    match = { class = "^(pulsemixer)$" },
    workspace = "special:media silent",
})
hl.window_rule({
    match = { class = "^(scratch-todo|scratch-glances|pulsemixer)$" },
    fullscreen_state = "1 0",
})

-- ─── Autostart ───────────────────────────────────────────────────────
hl.on("hyprland.start", function()
    hl.exec_cmd("hyprland-hyprpaper")
    hl.exec_cmd("systemctl --user restart waybar")
    hl.exec_cmd("sleep 0.5 && hyprland-cursor apply")
    hl.exec_cmd("sleep 0.5 && foot --server 2>/dev/null || true")
    hl.exec_cmd("sleep 1 && hyprland-wallpaper apply")
    hl.exec_cmd("swaync")
    hl.exec_cmd("systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP HYPRLAND_INSTANCE_SIGNATURE && systemctl --user restart swayosd-server")
    hl.exec_cmd("hypridle")
    hl.exec_cmd("hyprland-output-profile")
    hl.exec_cmd("sleep 2 && footclient --app-id=scratch-todo nvim " .. home .. "/.dotfiles/personal/TODO.md")
    hl.exec_cmd("sleep 4 && footclient --app-id=scratch-glances glances --time 5")
    hl.exec_cmd("sleep 5 && footclient --app-id=pulsemixer pulsemixer")

    -- Host-specific autostart (see hyprland-host.conf)
    hl.exec_cmd("steam")
    hl.exec_cmd("night-light")
end)

-- ─── App launch ──────────────────────────────────────────────────────
hl.bind(mod .. " + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mod .. " + D",      hl.dsp.exec_cmd(launcher))
hl.bind(mod .. " + E",      hl.dsp.exec_cmd(fileManager))
hl.bind(mod .. " + SHIFT + V", hl.dsp.exec_cmd("rofi-videos"))

-- ─── Screenshots / color picker ──────────────────────────────────────
hl.bind("Print",              hl.dsp.exec_cmd("screenshot output"))
hl.bind(mod .. " + SHIFT + S", hl.dsp.exec_cmd("screenshot region"))
hl.bind(mod .. " + Print",     hl.dsp.exec_cmd("screenshot window"))
hl.bind(mod .. " + SHIFT + P", hl.dsp.exec_cmd("hyprpicker -a -n"))

-- ─── Scratchpads ─────────────────────────────────────────────────────
hl.bind(mod .. " + apostrophe", hl.dsp.exec_cmd("hyprland-scratch-show todo"))
hl.bind(mod .. " + semicolon",  hl.dsp.exec_cmd("hyprland-scratch-show media"))

-- ─── Focus ───────────────────────────────────────────────────────────
hl.bind(mod .. " + J", hl.dsp.exec_cmd("hyprland-monocle cycle-next"))
hl.bind(mod .. " + K", hl.dsp.exec_cmd("hyprland-monocle cycle-prev"))
hl.bind(mod .. " + H", hl.dsp.exec_cmd("hyprland-output focus l"))
hl.bind(mod .. " + L", hl.dsp.exec_cmd("hyprland-output focus r"))
hl.bind(mod .. " + SHIFT + H", hl.dsp.exec_cmd("hyprland-output move  l"))
hl.bind(mod .. " + SHIFT + L", hl.dsp.exec_cmd("hyprland-output move  r"))

-- ─── Split resize + swap ─────────────────────────────────────────────
hl.bind(mod .. " + bracketleft",  hl.dsp.layout("splitratio -0.05"))
hl.bind(mod .. " + bracketright", hl.dsp.layout("splitratio +0.05"))
hl.bind(mod .. " + SHIFT + Return", hl.dsp.layout("swapsplit"))
hl.bind(mod .. " + P", hl.dsp.window.pseudo())

-- ─── Layout ──────────────────────────────────────────────────────────
hl.bind(mod .. " + F",         hl.dsp.exec_cmd("hyprland-monocle toggle"))
hl.bind(mod .. " + SHIFT + F", hl.dsp.window.fullscreen_state({ action = "toggle", internal = 2, client = 2 }))
hl.bind(mod .. " + T",         hl.dsp.exec_cmd("hyprland-float-toggle"))

-- ─── Window management ───────────────────────────────────────────────
hl.bind(mod .. " + Q",         hl.dsp.window.close())
hl.bind(mod .. " + SHIFT + R", hl.dsp.exec_cmd("hyprctl reload && sleep 0.5 && hyprland-output-profile --once && hyprland-ws-orphans"))
hl.bind(mod .. " + SHIFT + Q", hl.dsp.exit())
hl.bind(mod .. " + SHIFT + X", hl.dsp.exec_cmd("hyprlock"))

-- ─── Wallpaper + cursor pickers (preview-cycle submaps) ──────────────
hl.bind(mod .. " + SHIFT + W", hl.dsp.submap("wallpaper"))
hl.bind(mod .. " + SHIFT + C", function()
    hl.config({ cursor = { inactive_timeout = 0 } })
    hl.dispatch(hl.dsp.submap("cursor"))
end)

hl.define_submap("wallpaper", function()
    hl.bind("Right", hl.dsp.exec_cmd("hyprland-wallpaper next"))
    hl.bind("l",     hl.dsp.exec_cmd("hyprland-wallpaper next"))
    hl.bind("Down",  hl.dsp.exec_cmd("hyprland-wallpaper next"))
    hl.bind("j",     hl.dsp.exec_cmd("hyprland-wallpaper next"))
    hl.bind("Left",  hl.dsp.exec_cmd("hyprland-wallpaper prev"))
    hl.bind("h",     hl.dsp.exec_cmd("hyprland-wallpaper prev"))
    hl.bind("Up",    hl.dsp.exec_cmd("hyprland-wallpaper prev"))
    hl.bind("k",     hl.dsp.exec_cmd("hyprland-wallpaper prev"))
    hl.bind("p",     hl.dsp.exec_cmd("hyprland-wallpaper pywal-toggle"))
    hl.bind("Return", function()
        hl.dispatch(hl.dsp.exec_cmd("hyprland-wallpaper commit"))
        hl.dispatch(hl.dsp.submap("reset"))
    end)
    hl.bind("Escape", function()
        hl.dispatch(hl.dsp.exec_cmd("hyprland-wallpaper revert"))
        hl.dispatch(hl.dsp.submap("reset"))
    end)
    hl.bind("q", function()
        hl.dispatch(hl.dsp.exec_cmd("hyprland-wallpaper revert"))
        hl.dispatch(hl.dsp.submap("reset"))
    end)
end)

hl.define_submap("cursor", function()
    hl.bind("Right", hl.dsp.exec_cmd("hyprland-cursor next"))
    hl.bind("l",     hl.dsp.exec_cmd("hyprland-cursor next"))
    hl.bind("Down",  hl.dsp.exec_cmd("hyprland-cursor next"))
    hl.bind("j",     hl.dsp.exec_cmd("hyprland-cursor next"))
    hl.bind("Left",  hl.dsp.exec_cmd("hyprland-cursor prev"))
    hl.bind("h",     hl.dsp.exec_cmd("hyprland-cursor prev"))
    hl.bind("Up",    hl.dsp.exec_cmd("hyprland-cursor prev"))
    hl.bind("k",     hl.dsp.exec_cmd("hyprland-cursor prev"))
    hl.bind("Return", function()
        hl.dispatch(hl.dsp.exec_cmd("hyprland-cursor commit"))
        hl.config({ cursor = { inactive_timeout = 2 } })
        hl.dispatch(hl.dsp.submap("reset"))
    end)
    hl.bind("Escape", function()
        hl.dispatch(hl.dsp.exec_cmd("hyprland-cursor revert"))
        hl.config({ cursor = { inactive_timeout = 2 } })
        hl.dispatch(hl.dsp.submap("reset"))
    end)
    hl.bind("q", function()
        hl.dispatch(hl.dsp.exec_cmd("hyprland-cursor revert"))
        hl.config({ cursor = { inactive_timeout = 2 } })
        hl.dispatch(hl.dsp.submap("reset"))
    end)
end)

-- ─── Workspaces ──────────────────────────────────────────────────────
for i = 1, 9 do
    hl.bind(mod .. " + " .. i,         hl.dsp.exec_cmd("hyprland-ws switch " .. i))
    hl.bind(mod .. " + SHIFT + " .. i, hl.dsp.exec_cmd("hyprland-ws move " .. i))
end

-- ─── Floating with mouse ─────────────────────────────────────────────
hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- ─── Media / volume / brightness keys ────────────────────────────────
hl.bind("XF86AudioRaiseVolume",  hl.dsp.exec_cmd("volume up"),   { locked = true })
hl.bind("XF86AudioLowerVolume",  hl.dsp.exec_cmd("volume down"), { locked = true })
hl.bind("XF86AudioMute",         hl.dsp.exec_cmd("volume mute"), { locked = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightness up"),   { locked = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightness down"), { locked = true })
hl.bind("XF86AudioPlay",         hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioNext",         hl.dsp.exec_cmd("playerctl next"),       { locked = true })
hl.bind("XF86AudioPrev",         hl.dsp.exec_cmd("playerctl previous"),   { locked = true })
