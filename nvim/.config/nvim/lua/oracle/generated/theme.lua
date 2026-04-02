local path = vim.fn.expand("~/.local/state/loom/generated/nvim-theme.lua")

local ok, result = pcall(dofile, path)
if not ok then
  vim.schedule(function()
    vim.notify("Failed to load loom theme: " .. tostring(result), vim.log.levels.WARN)
  end)
  return {}
end

return result or {}
