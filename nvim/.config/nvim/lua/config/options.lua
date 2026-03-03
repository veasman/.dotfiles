local opt = vim.opt

vim.g.mapleader = " "
vim.g.maplocalleader = ","

opt.number = true
opt.relativenumber = true
opt.cursorline = true
opt.signcolumn = "yes"

-- Default: 4 spaces
opt.expandtab = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.softtabstop = 4
opt.smartindent = true

opt.wrap = false
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.colorcolumn = "120"

opt.swapfile = false
opt.backup = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("state") .. "/undo"

opt.hlsearch = false
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true

opt.termguicolors = true
opt.clipboard = "unnamedplus"

opt.updatetime = 200
opt.timeoutlen = 400

vim.diagnostic.config({
  virtual_text = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  float = { border = "rounded" },
})
