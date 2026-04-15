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
        "shaunsingh/nord.nvim",
        priority = 1000,
        config = function()
            vim.g.nord_disable_background = true
            vim.g.nord_italic = false
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

    -- kara.nvim: extracted from oracle.core.kara_theme into its own
    -- repo at github.com/veasman/kara.nvim. During local development
    -- we point at the working copy in ~/repos/kara.nvim via lazy's
    -- `dir` option; flip to the github spec once the repo is pushed.
    {
        dir = vim.fn.expand("~/repos/kara.nvim"),
        name = "kara.nvim",
        lazy = false,
        priority = 1001, -- higher than the dependency colorschemes so
                         -- kara's setup runs first and dispatches into
                         -- them rather than the other way around
        dependencies = {
            "shaunsingh/nord.nvim",
            "ellisonleao/gruvbox.nvim",
            "vague-theme/vague.nvim",
            "echasnovski/mini.base16",
        },
        config = function()
            -- setup() internally calls apply() which already issues
            -- the right :colorscheme for the generated palette
            -- (nord/gruvbox/vague/kara-custom). Do NOT also run
            -- `vim.cmd.colorscheme("kara")` here — that triggers
            -- colors/kara.vim which calls apply() a second time,
            -- doubling startup cost.
            require("kara").setup({
                auto_reload = true,
                serverstart = true,
            })
        end,
    },
}
