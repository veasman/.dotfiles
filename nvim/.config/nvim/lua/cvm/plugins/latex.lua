return {
    {
        "lervag/vimtex",
        lazy = false,
        init = function()
            vim.g.vimtex_view_method = "zathura"
            vim.g.vimtex_compiler_method = "latexmk"
            vim.g.vimtex_quickfix_mode = 0
        end,
        keys = {
            { "<leader>ls", "<cmd>VimtexCompile<CR>", desc = "LaTeX compile" },
            { "<leader>lv", "<cmd>VimtexView<CR>", desc = "LaTeX view" },
            { "<leader>lc", "<cmd>VimtexClean<CR>", desc = "LaTeX clean" },
            { "<leader>le", "<cmd>VimtexErrors<CR>", desc = "LaTeX errors" },
            { "<leader>lt", "<cmd>VimtexTocOpen<CR>", desc = "LaTeX TOC" },
        },
    },
}
