local opts = { noremap = true, silent = true }

---@diagnostic disable-next-line: unused-local
local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Align --
vim.keymap.set('x', 'aa', function() require 'align'.align_to_char(1, true) end, opts) -- Aligns to 1 character, looking left
vim.keymap.set('x', 'as', function() require 'align'.align_to_char(2, true, true) end, opts) -- Aligns to 2 characters, looking left and with previews
vim.keymap.set('x', 'aw', function() require 'align'.align_to_string(false, true, true) end, opts) -- Aligns to a string, looking left and with previews
vim.keymap.set('x', 'ar', function() require 'align'.align_to_string(true, true, true) end, opts) -- Aligns to a Lua pattern, looking left and with previews

-- Example gawip to align a paragraph to a string, looking left and with previews
vim.keymap.set(
    'n',
    'gaw',
    function()
        local a = require 'align'
        a.operator(
            a.align_to_string,
            { is_pattern = false, reverse = true, preview = true }
        )
    end,
    opts
)

-- Example gaaip to aling a paragraph to 1 character, looking left
vim.keymap.set(
    'n',
    'gaa',
    function()
        local a = require 'align'
        a.operator(
            a.align_to_char,
            { reverse = true }
        )
    end,
    opts
)

-- Trouble --
keymap("n", "<leader>xx", "<cmd>Trouble<cr>", opts)
keymap("n", "<leader>xw", "<cmd>Trouble workspace_diagnostics<cr>", opts)
keymap("n", "<leader>xd", "<cmd>Trouble document_diagnostics<cr>", opts)
keymap("n", "<leader>xl", "<cmd>Trouble loclist<cr>", opts)
keymap("n", "<leader>xq", "<cmd>Trouble quickfix<cr>", opts)
keymap("n", "gR", "<cmd>Trouble lsp_references<cr>", opts)

-- Refactoring --
-- Remaps for the refactoring operations currently offered by the plugin
keymap("v", "<leader>re", "<esc><cmd>lua require('refactoring').refactor('Extract Function')<cr>", opts)
keymap("v", "<leader>rf", "<esc><cmd>lua require('refactoring').refactor('Extract Function To File')<cr>", opts)
keymap("v", "<leader>rv", "<esc><cmd>lua require('refactoring').refactor('Extract Variable')<cr>", opts)
keymap("v", "<leader>ri", "<esc><cmd>lua require('refactoring').refactor('Inline Variable')<cr>", opts)

-- Extract block doesn't need visual mode
keymap("n", "<leader>rb", "<cmd>lua require('refactoring').refactor('Extract Block')<cr>", opts)
keymap("n", "<leader>rbf", "<cmd>lua require('refactoring').refactor('Extract Block To File')<cr>", opts)

-- Inline variable can also pick up the identifier currently under the cursor without visual mode
keymap("n", "<leader>ri", "<cmd>lua require('refactoring').refactor('Inline Variable')<cr>", opts)

-- Spectre --
keymap("n", "<leader>S", "<cmd>lua require('spectre').open()<cr>", opts)
keymap("n", "<leader>Sw", "<cmd>lua require('spectre').open_visual({select_word=true})<cr>", opts)
keymap("n", "<leader>Sf", "<cmd>lua require('spectre').open_file_search()<cr>", opts)

-- Renamer --
keymap("i", "<C-R>", "<cmd>lua require('renamer').rename()<cr>", opts)
keymap("n", "<leader>rn", "<cmd>lua require('renamer').rename()<cr>", opts)
keymap("v", "<leader>rn", "<cmd>lua require('renamer').rename()<cr>", opts)

-- NvimTree --
keymap("n", "<leader>op", ":NvimTreeToggle<cr>", opts)
keymap("n", "<leader>tf", ":NvimTreeFindFile<cr>", opts)

-- Intellisense --
keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<cr>", opts)
keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<cr>", opts)
keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<cr>", opts)
keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<cr>", opts)
keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)
keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<cr>", opts)
keymap("n", "<C-n>", "<cmd>lua vim.diagnostic.goto_prev()<cr>", opts)
keymap("n", "<C-p>", "<cmd>lua vim.diagnostic.goto_next()<cr>", opts)
keymap("n", "<leader>lf", "<cmd>lua vim.lsp.buf.format({ async = true })<cr>", opts)

-- Undotree --
keymap("n", "<leader>u", ":UndotreeToggle<cr>", opts)

-- Fugitive --
keymap("n", "<leader>gs", "<cmd>lua require('neogit').open()<cr>", opts)
keymap("n", "<leader>gc", "<cmd>lua require('neogit').open({\"commit\"})<cr>", opts)
keymap("n", "<leader>gd", "<cmd>Gvdiff<cr>", opts)
keymap("n", "<leader>gh", "<cmd>diffget //2<cr>", opts)
keymap("n", "<leader>gl", "<cmd>diffget //3<cr>", opts)

-- Harpoon --
keymap("n", "<leader>ha", "<cmd>lua require('harpoon.mark').add_file()<cr>", opts)
keymap("n", "<leader>hc", "<cmd>lua require('harpoon.mark').clear_all()<cr>", opts)
keymap("n", "<leader>ht", "<cmd>lua require('harpoon.ui').toggle_quick_menu()<cr>", opts)
keymap("n", "<leader>a", "<cmd>lua require('harpoon.ui').nav_file(1)<cr>", opts)
keymap("n", "<leader>s", "<cmd>lua require('harpoon.ui').nav_file(2)<cr>", opts)
keymap("n", "<leader>d", "<cmd>lua require('harpoon.ui').nav_file(3)<cr>", opts)
keymap("n", "<leader>f", "<cmd>lua require('harpoon.ui').nav_file(4)<cr>", opts)

-- Telescope --
keymap("n", "<leader><leader>", "<cmd>lua require('telescope.builtin').find_files()<cr>", opts)
keymap("n", "<leader>sp", "<cmd>lua require('telescope.builtin').live_grep()<cr>", opts)
keymap("n", "<leader>fb", "<cmd>lua require('telescope.builtin').git_branches()<cr>", opts)
keymap("n", "<leader>fc", "<cmd>lua require('telescope.builtin').git_commits()<cr>", opts)
keymap("n", "<leader>hh", "<cmd>lua require('telescope.builtin').commands()<cr>", opts)
keymap("n", "<leader>vh", "<cmd>lua require('telescope.builtin').help_tags()<cr>", opts)
keymap("n", "<leader>vb", "<cmd>lua require('telescope.builtin').buffers()<cr>", opts)

-- Bufferline --
keymap("n", "L", "<cmd>BufferLineCycleNext<cr>", opts)
keymap("n", "H", "<cmd>BufferLineCyclePrev<cr>", opts)
keymap("n", "<leader>bc", "<cmd>BufferLinePickClose<cr>", opts)

-- Code actions --
keymap("n", "<C-a>", ":CodeActionMenu<cr>", opts)

-- Formatter --
keymap("n", "<leader>F", ":Format<cr>", opts)

-- Misc --

-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Y acts like other capitals
keymap("n", "Y", "y$", opts)

-- Keep cursor centered
keymap("n", "n", "nzzzv", opts)
keymap("n", "N", "Nzzzv", opts)
keymap("n", "J", "mzJ'z", opts)

-- Undo break points
keymap("i", ",", ",<c-g>u", opts)
keymap("i", ".", ".<c-g>u", opts)
keymap("i", "!", "!<c-g>u", opts)
keymap("i", "?", "?<c-g>u", opts)

-- Jumplist mutations
keymap("n", "<expr>j", "(v:count > 5 ? \"m\'\" . v:count : \"\") . \'j\'", opts)
keymap("n", "<expr>k", "(v:count > 5 ? \"m\'\" . v:count : \"\") . \'k\'", opts)

-- Moving text around
keymap("v", "J", ":m '>+1<cr>gv=gv", opts)
keymap("v", "K", ":m '<-2<cr>gv=gv", opts)
keymap("v", "p", '"_dP', opts)
--inoremap <C-j> <esc>:m .+1<cr>==
--inoremap <C-k> <esc>:m .-2<cr>==
--nnoremap <leader>j :m .+1<cr>==
--nnoremap <leader>k :m .-2<cr>==
