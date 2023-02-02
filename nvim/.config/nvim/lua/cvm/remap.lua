local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

local keymap = vim.keymap.set

vim.g.mapleader = " "

keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")

keymap("n", "J", "mzJ`z")
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")
keymap("n", "n", "nzzzv")
keymap("n", "N", "Nzzzv")

keymap("n", "<leader>vwm", function()
    require("vim-with-me").StartVimWithMe()
end)
keymap("n", "<leader>svwm", function()
    require("vim-with-me").StopVimWithMe()
end)

-- greatest remap ever
keymap("x", "<leader>p", [["_dP]])

-- next greatest remap ever : asbjornHaland
keymap({"n", "v"}, "<leader>y", [["+y]])
keymap("n", "<leader>Y", [["+Y]])

keymap({"n", "v"}, "<leader>d", [["_d]])

-- This is going to get me cancelled
keymap("i", "<C-c>", "<Esc>")

keymap("n", "Q", "<nop>")
keymap("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
keymap("n", "<leader>f", vim.lsp.buf.format)

keymap("n", "<C-k>", "<cmd>cnext<CR>zz")
keymap("n", "<C-j>", "<cmd>cprev<CR>zz")
keymap("n", "<leader>k", "<cmd>lnext<CR>zz")
keymap("n", "<leader>j", "<cmd>lprev<CR>zz")

keymap("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
keymap("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

-- Git


-- Harpoon
keymap("n", "<leader>a", mark.add_file)
keymap("n", "<C-e>", ui.toggle_quick_menu)

keymap("n", "<C-h>", function() ui.nav_file(1) end)
keymap("n", "<C-j>", function() ui.nav_file(2) end)
keymap("n", "<C-k>", function() ui.nav_file(3) end)
keymap("n", "<C-l>", function() ui.nav_file(4) end)

-- Open
keymap("n", "<leader>op", "<cmd>NvimTreeFindFileToggle<cr>", { desc = "Project sidebar" })
--keymap("n", "<leader>ot", "<cmd>NvimTreeToggle<cr>", { desc = "Project sidebar" })

-- Rename
keymap("n", "<leader>rs", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Rename symbol" })

-- Search
keymap("n", "<leader>sp", "<cmd>lua require('telescope.builtin').git_files()<cr>", { desc = "Search project" })
