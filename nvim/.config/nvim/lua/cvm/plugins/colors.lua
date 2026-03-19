local ok, theme = pcall(require, "cvm.generated.theme")

if not ok then
    theme = {
        colorscheme = "rose-pine",
        variant = "main",
        transparent = true,
    }
end

return {
    {
        "rose-pine/neovim",
        name = "rose-pine",
        priority = 1000,
        config = function()
            require("rose-pine").setup({
                variant = theme.variant,
                disable_background = theme.transparent,
                styles = {
                    italic = false,
                },
            })

            if theme.colorscheme == "rose-pine" then
                vim.cmd.colorscheme("rose-pine")
            end
        end,
    },

    {
        "ellisonleao/gruvbox.nvim",
        lazy = true,
        config = function()
            if theme.colorscheme == "gruvbox" then
                vim.cmd.colorscheme("gruvbox")
            end
        end,
    },

    {
        "nyoom-engineering/oxocarbon.nvim",
        lazy = true,
        config = function()
            if theme.colorscheme == "oxocarbon" then
                vim.cmd.colorscheme("oxocarbon")
            end
        end,
    },

    {
        "catppuccin/nvim",
        name = "catppuccin",
        lazy = true,
        config = function()
            require("catppuccin").setup({
                flavour = theme.variant,
                transparent_background = theme.transparent,
                no_italic = true,
            })

            if theme.colorscheme == "catppuccin" then
                vim.cmd.colorscheme("catppuccin")
            end
        end,
    },

    {
        "EdenEast/nightfox.nvim",
        lazy = true,
        config = function()
            if theme.colorscheme == "dayfox" then
                vim.cmd.colorscheme("dayfox")
            elseif theme.colorscheme == "dawnfox" then
                vim.cmd.colorscheme("dawnfox")
            end
        end,
    },

    {
        "vague-theme/vague.nvim",
        lazy = true,
        config = function()
            require("vague").setup({
                transparent = theme.transparent,
            })
            if theme.colorscheme == "vague" then
                vim.cmd("colorscheme vague")
            end
        end,
    },

    {
        "paulfrische/reddish.nvim",
        lazy = true,
        config = function()
            if theme.colorscheme == "reddish" then
                vim.cmd("colorscheme reddish")
            end
        end,
    },

    {
        "folke/tokyonight.nvim",
        lazy = true,
        config = function()
            if theme.colorscheme == "tokyonight" then
                vim.cmd("colorscheme tokyonight")
            end
        end,
    },

    {
        "echasnovski/mini.base16",
        version = false,
        lazy = true,
        config = function()
            if theme.colorscheme == "loom-custom" and theme.palette then
                require("mini.base16").setup({
                    palette = theme.palette,
                    use_cterm = true,
                })
            end
        end,
    },
}
