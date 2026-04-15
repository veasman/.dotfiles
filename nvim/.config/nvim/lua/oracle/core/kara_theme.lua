local M = {}

local function load_theme()
  package.loaded["oracle.generated.theme"] = nil

  local ok, theme = pcall(require, "oracle.generated.theme")
  if not ok then
    vim.schedule(function()
      vim.notify("Failed to load kara theme: " .. tostring(theme), vim.log.levels.ERROR)
    end)
    return nil
  end

  if type(theme) ~= "table" then
    vim.schedule(function()
      vim.notify("Kara theme payload is not a table", vim.log.levels.ERROR)
    end)
    return nil
  end

  return theme
end

local function set_hl(group, spec)
  vim.api.nvim_set_hl(0, group, spec)
end

local function apply_transparency()
  set_hl("Normal", { bg = "NONE" })
  set_hl("NormalNC", { bg = "NONE" })
  set_hl("NormalFloat", { bg = "NONE" })
  set_hl("SignColumn", { bg = "NONE" })
  set_hl("EndOfBuffer", { bg = "NONE" })
end

local function apply_semantic_overrides(theme)
  if type(theme.palette) ~= "table" then
    return
  end

  local p = theme.palette
  local is_light = theme.variant == "light"

  if is_light then
    set_hl("CursorLine", { bg = p.base02 })
    set_hl("CursorColumn", { bg = p.base02 })
    set_hl("CursorLineNr", { fg = p.base0D, bg = p.base02, bold = true })
    set_hl("StatusLine", { fg = p.base06, bg = p.base02, bold = true })
    set_hl("StatusLineNC", { fg = p.base03, bg = p.base01 })
    set_hl("TabLine", { fg = p.base04, bg = p.base01 })
    set_hl("TabLineSel", { fg = p.base07, bg = p.base0D, bold = true })
    set_hl("Pmenu", { fg = p.base05, bg = p.base01 })
    set_hl("PmenuSel", { fg = p.base07, bg = p.base0D, bold = true })
    set_hl("Visual", { fg = p.base07, bg = p.base0D })
    set_hl("Search", { fg = p.base07, bg = p.base09, bold = true })
    set_hl("IncSearch", { fg = p.base00, bg = p.base0A, bold = true })

    set_hl("TelescopeNormal", { fg = p.base05, bg = p.base01 })
    set_hl("TelescopeBorder", { fg = p.base03, bg = p.base01 })
    set_hl("TelescopeTitle", { fg = p.base0D, bg = p.base01, bold = true })
    set_hl("TelescopeSelection", { fg = p.base07, bg = p.base0D, bold = true })
    set_hl("TelescopeMatching", { fg = p.base08, bold = true })

    set_hl("WinSeparator", { fg = p.base03, bg = "NONE" })
    set_hl("VertSplit", { fg = p.base03, bg = "NONE" })
    set_hl("LineNr", { fg = p.base04, bg = "NONE" })
  else
    set_hl("CursorLine", { bg = p.base01 })
    set_hl("CursorColumn", { bg = p.base01 })
    set_hl("CursorLineNr", { fg = p.base0A, bg = p.base01, bold = true })
    set_hl("StatusLine", { fg = p.base05, bg = p.base01, bold = true })
    set_hl("StatusLineNC", { fg = p.base03, bg = p.base01 })
    set_hl("TabLine", { fg = p.base03, bg = p.base01 })
    set_hl("TabLineSel", { fg = p.base00, bg = p.base0D, bold = true })
    set_hl("Pmenu", { fg = p.base05, bg = p.base01 })
    set_hl("PmenuSel", { fg = p.base00, bg = p.base0D, bold = true })

    set_hl("TelescopeNormal", { fg = p.base05, bg = p.base01 })
    set_hl("TelescopeBorder", { fg = p.base03, bg = p.base01 })
    set_hl("TelescopeTitle", { fg = p.base0D, bg = p.base01, bold = true })
    set_hl("TelescopeSelection", { fg = p.base00, bg = p.base0D, bold = true })
    set_hl("TelescopeMatching", { fg = p.base0A, bold = true })
  end
end

local function apply_gruvbox(theme)
  local ok, gruvbox = pcall(require, "gruvbox")
  if ok then
    gruvbox.setup({
      transparent_mode = theme.transparent == true,
      italic = {
        strings = false,
        comments = false,
        operators = false,
        folds = false,
      },
    })
  end

  vim.cmd.colorscheme("gruvbox")

  if theme.transparent then
    apply_transparency()
  end
end

local function apply_vague(theme)
  local ok, vague = pcall(require, "vague")
  if ok then
    vague.setup({
      transparent = theme.transparent == true,
    })
  end

  vim.cmd.colorscheme("vague")

  if theme.transparent then
    apply_transparency()
  end
end

local function apply_kara_custom(theme)
  local ok, mini = pcall(require, "mini.base16")
  if not ok then
    vim.schedule(function()
      vim.notify("mini.base16 is not available for kara-custom", vim.log.levels.ERROR)
    end)
    return
  end

  if type(theme.palette) ~= "table" then
    vim.schedule(function()
      vim.notify("Kara theme missing palette for kara-custom", vim.log.levels.ERROR)
    end)
    return
  end

  mini.setup({
    palette = theme.palette,
    use_cterm = true,
  })

  vim.g.colors_name = "kara-custom"

  if theme.transparent then
    apply_transparency()
  end

  apply_semantic_overrides(theme)
end

function M.apply()
  local theme = load_theme()
  if not theme then
    return
  end

  vim.o.termguicolors = true

  if theme.colorscheme == "gruvbox" then
    apply_gruvbox(theme)
    return
  end

  if theme.colorscheme == "vague" then
    apply_vague(theme)
    return
  end

  if theme.colorscheme == "kara-custom" then
    apply_kara_custom(theme)
    return
  end

  local ok = pcall(vim.cmd.colorscheme, theme.colorscheme)
  if not ok then
    vim.schedule(function()
      vim.notify("Unknown Kara colorscheme: " .. tostring(theme.colorscheme), vim.log.levels.ERROR)
    end)
  end
end

function M.setup()
  vim.api.nvim_create_user_command("KaraReloadTheme", function()
    M.apply()
  end, {})

  -- Expose a predictable Unix socket so `kara-beautify apply` can push
  -- a live reload into this nvim instance. kara-beautify/src/reload.rs
  -- iterates /tmp/nvim-kara-*.sock and sends :KaraReloadTheme via
  -- nvim --server --remote-send. nvim always has its own per-session
  -- socket via $NVIM_LISTEN_ADDRESS, but the pattern is unpredictable;
  -- this gives kara a stable glob to target.
  local sock = string.format("/tmp/nvim-kara-%d.sock", vim.fn.getpid())
  pcall(vim.fn.serverstart, sock)

  -- Clean up our socket on exit so stale files don't accumulate in /tmp.
  vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
      pcall(vim.fn.delete, sock)
    end,
  })

  M.apply()
end

return M
