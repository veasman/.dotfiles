local opt = vim.opt
local fn = vim.fn

opt.guicursor = ""
opt.number = true
opt.relativenumber = true
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true
opt.wrap = false
opt.swapfile = false
opt.backup = false
opt.undofile = true
opt.hlsearch = false
opt.incsearch = true
opt.termguicolors = true
opt.scrolloff = 8
opt.signcolumn = "yes"
opt.updatetime = 250
opt.timeoutlen = 400
opt.colorcolumn = "120"
opt.clipboard = "unnamedplus"
opt.splitright = true
opt.splitbelow = true
opt.ignorecase = true
opt.smartcase = true
opt.mouse = "a"
opt.cursorline = true
opt.showmode = false
opt.laststatus = 3
opt.cmdheight = 1
opt.statusline = " %f %m%r%h%w %= %y  %l:%c  %p%% "

opt.isfname:append("@-@")

vim.cmd("highlight Normal guibg=None")

local undodir = fn.stdpath("state") .. "/undo"
if fn.isdirectory(undodir) == 0 then
    fn.mkdir(undodir, "p")
end
opt.undodir = undodir
