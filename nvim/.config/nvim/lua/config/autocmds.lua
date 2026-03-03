local aug = vim.api.nvim_create_augroup("cvm_core", { clear = true })

-- Highlight yank
vim.api.nvim_create_autocmd("TextYankPost", {
  group = aug,
  callback = function()
    vim.highlight.on_yank({ timeout = 120 })
  end,
})

-- Trim trailing whitespace on save
vim.api.nvim_create_autocmd("BufWritePre", {
  group = aug,
  pattern = "*",
  command = [[%s/\s\+$//e]],
})

-- JS/TS/Next: 2 spaces
vim.api.nvim_create_autocmd("FileType", {
  group = aug,
  pattern = { "javascript", "javascriptreact", "typescript", "typescriptreact", "tsx" },
  callback = function()
    vim.opt_local.expandtab = true
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.softtabstop = 2
  end,
})

-- Go: tabs (gofmt will enforce anyway)
vim.api.nvim_create_autocmd("FileType", {
  group = aug,
  pattern = { "go" },
  callback = function()
    vim.opt_local.expandtab = false
    vim.opt_local.tabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.softtabstop = 4
  end,
})
