vim.g.mapleader = " "
vim.g.maplocalleader = " "

local keymap = vim.keymap.set
local opts = { silent = true }

keymap("n", "-", "<cmd>Oil<CR>", { desc = "Open parent directory" })
keymap("n", "<leader>pv", "<cmd>Oil<CR>", { desc = "File explorer" })

keymap("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
keymap("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })
keymap("n", "J", "mzJ`z", { desc = "Join lines keep cursor" })
keymap("n", "<C-d>", "<C-d>zz", { desc = "Half-page down centered" })
keymap("n", "<C-u>", "<C-u>zz", { desc = "Half-page up centered" })
keymap("n", "n", "nzzzv", { desc = "Next search centered" })
keymap("n", "N", "Nzzzv", { desc = "Prev search centered" })

keymap("x", "<leader>p", [['_dP]], { desc = "Paste without yanking" })
keymap({ "n", "v" }, "<leader>y", [["+y]], { desc = "Yank to system clipboard" })
keymap("n", "<leader>Y", [["+Y]], { desc = "Yank line to system clipboard" })
keymap({ "n", "v" }, "<leader>d", [["_d]], { desc = "Delete without yanking" })

keymap("i", "<C-c>", "<Esc>", { desc = "Escape insert mode" })
keymap("n", "Q", "<nop>")
keymap("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
keymap("n", "<leader>f", function()
    vim.lsp.buf.format({ async = true })
end, { desc = "Format buffer" })

keymap("n", "<C-k>", "<cmd>cnext<CR>zz", { desc = "Quickfix next" })
keymap("n", "<C-j>", "<cmd>cprev<CR>zz", { desc = "Quickfix prev" })
keymap("n", "<leader>k", "<cmd>lnext<CR>zz", { desc = "Loclist next" })
keymap("n", "<leader>j", "<cmd>lprev<CR>zz", { desc = "Loclist prev" })

keymap("n", "<leader>s", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Substitute word under cursor" })
keymap("n", "<leader>x", "<cmd>!chmod +x %<CR>", { desc = "Make file executable" })

-- keymap("n", "<leader><leader>", function()
    -- vim.cmd("so")
-- end, { desc = "Source current file" })

keymap("n", "<leader>bd", "<cmd>bdelete<CR>", { desc = "Delete buffer" })
