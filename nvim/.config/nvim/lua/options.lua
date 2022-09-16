local o = vim.o

o.guicursor = ""
o.tabstop = 4
o.softtabstop = 4
o.shiftwidth = 4
o.expandtab = true
o.smartindent = true
o.number = true
o.relativenumber = true
o.wrap = false
o.hlsearch = false
o.swapfile = false
o.undodir = os.getenv("HOME") .. '/.local/share/nvim/undodir'
o.undofile = true
o.scrolloff = 8
o.completeopt = "menuone,noinsert,noselect"
o.signcolumn = "yes"
o.termguicolors = true
o.secure = true
--o.cmdheight = 2
o.updatetime = 50
o.shortmess = "c"
o.colorcolumn = "120"
vim.opt.list = true

-- NvCode --
vim.g.nvcode_termcolors = 256

-- Emmet --
vim.g.user_emmet_mode = 'n'
vim.g.user_emmet_leader_key = ','

vim.cmd [[ colorscheme onedark ]]
vim.cmd [[ highlight DiffChange gui=None ]]
vim.cmd [[ let g:vimwiki_list = [{ 'path': '~/.local/share/nvim/vimwiki/' }] ]]
