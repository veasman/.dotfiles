local map = vim.keymap.set

-- sanity
map("n", "<Esc>", "<cmd>nohlsearch<cr>")

-- splits
map("n", "<leader>sv", "<cmd>vsplit<cr>", { desc = "Split vertical" })
map("n", "<leader>sh", "<cmd>split<cr>", { desc = "Split horizontal" })
map("n", "<leader>se", "<C-w>=", { desc = "Equalize splits" })
map("n", "<leader>sx", "<cmd>close<cr>", { desc = "Close split" })

-- buffers
map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete buffer" })
map("n", "<leader>bn", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bprevious<cr>", { desc = "Prev buffer" })

-- quickfix
map("n", "<leader>qo", "<cmd>copen<cr>", { desc = "Quickfix open" })
map("n", "<leader>qc", "<cmd>cclose<cr>", { desc = "Quickfix close" })

-- diagnostics
map("n", "<leader>e", vim.diagnostic.open_float, { desc = "Diagnostic float" })
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Prev diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
