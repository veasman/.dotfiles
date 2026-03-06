return {
    "folke/trouble.nvim",
    cmd = "Trouble",
    keys = {
        { "<leader>xx", "<cmd>Trouble diagnostics toggle<CR>", desc = "Diagnostics" },
        { "<leader>xq", "<cmd>Trouble qflist toggle<CR>", desc = "Quickfix" },
        { "<leader>xl", "<cmd>Trouble loclist toggle<CR>", desc = "Loclist" },
    },
    opts = {
        use_diagnostic_signs = true,
    },
}
