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
        "marko-cerovac/material.nvim",
        lazy = true,
        init = function()
            vim.g.material_style = theme.variant
        end,
        config = function()
            if theme.colorscheme == "material" then
                vim.cmd.colorscheme("material")
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
}
