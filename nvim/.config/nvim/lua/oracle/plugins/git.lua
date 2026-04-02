return {
    {
        "tpope/vim-fugitive",
        cmd = { "Git", "Gdiffsplit", "Gvdiffsplit", "Gwrite", "Gread", "Ggrep" },
        keys = {
            { "<leader>gs", vim.cmd.Git },
        },
    },
    {
        "mbbill/undotree",
        cmd = "UndotreeToggle",
        keys = {
            { "<leader>u", vim.cmd.UndotreeToggle, desc = "Undo tree" },
        },
    },
}
