-- Thin shim that dofile's the kara-beautify nvim palette and returns it.
-- kara-beautify writes this file on every `kara-beautify apply`.
-- If it doesn't exist yet (beautify never run), returns an empty table
-- so nvim still starts cleanly; oracle.core.kara_theme handles the
-- empty case by skipping theme application.

local path = vim.fn.expand("~/.local/state/kara/generated/nvim-theme.lua")

local ok, result = pcall(dofile, path)
if not ok then
  vim.schedule(function()
    vim.notify("Failed to load kara theme: " .. tostring(result), vim.log.levels.WARN)
  end)
  return {}
end

return result or {}
