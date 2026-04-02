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
        end,
    },

    {
        "ellisonleao/gruvbox.nvim",
        priority = 1000,
        config = function()
            require("gruvbox").setup({
                transparent_mode = true,
                italic = {
                    strings = false,
                    comments = false,
                    operators = false,
                    folds = false,
                },
            })
        end,
    },

    {
        "nyoom-engineering/oxocarbon.nvim",
        lazy = true,
    },

    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        config = function()
            require("catppuccin").setup({
                flavour = "mocha",
                transparent_background = true,
                no_italic = true,
            })
        end,
    },

    {
        "EdenEast/nightfox.nvim",
        lazy = true,
    },

    {
        "vague-theme/vague.nvim",
        priority = 1000,
        config = function()
            require("vague").setup({
                transparent = true,
            })
        end,
    },

    {
        "paulfrische/reddish.nvim",
        lazy = true,
    },

    {
        "folke/tokyonight.nvim",
        lazy = true,
    },

    {
        "echasnovski/mini.base16",
        version = false,
        priority = 1000,
    },
}
