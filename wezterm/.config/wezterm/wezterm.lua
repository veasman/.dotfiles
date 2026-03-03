local wezterm = require 'wezterm'

return {
  font = wezterm.font("FiraCode Nerd Font"),
  font_size = 13.0,

  initial_cols = 120,
  initial_rows = 32,

  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },

  window_background_opacity = 0.95,
  enable_tab_bar = false,
  use_fancy_tab_bar = false,

  -- NVIDIA/X11 safe default
  front_end = "OpenGL",

  -- Commented out because this is overridden by zsh
  -- default_cursor_style = "SteadyBlock",

  -- Stop inactive dimming (often what makes cursor/colors look “different” when unfocused)
  inactive_pane_hsb = {
    saturation = 1.0,
    brightness = 1.0,
  },

  scrollback_lines = 10000,

  colors = {
    foreground = "#c9d1d9",
    background = "#0f1117",

    cursor_bg = "#58a6ff",
    cursor_fg = "#0f1117",
    cursor_border = "#58a6ff",

    selection_bg = "#264f78",
    selection_fg = "#ffffff",

    ansi = {
      "#484f58",
      "#ff7b72",
      "#3fb950",
      "#d29922",
      "#58a6ff",
      "#bc8cff",
      "#39c5cf",
      "#b1bac4",
    },

    brights = {
      "#6e7681",
      "#ffa198",
      "#56d364",
      "#e3b341",
      "#79c0ff",
      "#d2a8ff",
      "#56d4dd",
      "#ffffff",
    },
  },

  -- Keep it snappy and reduce pointless animation work
  animation_fps = 1,
}
