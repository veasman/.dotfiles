return {
    {
        "rose-pine/neovim",
        name = "rose-pine",
        priority = 1000,
        config = function()
            require("rose-pine").setup({
                disable_background = true,
                styles = {
                    italic = false,
                },
            })
            vim.cmd.colorscheme("rose-pine")
        end,
    },
    { "ellisonleao/gruvbox.nvim", lazy = true },
    {
        "marko-cerovac/material.nvim",
        lazy = true,
        init = function()
            vim.g.material_style = "deep ocean"
        end,
    },
    { "nyoom-engineering/oxocarbon.nvim", lazy = true },
}
