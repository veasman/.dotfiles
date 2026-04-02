local M = {}

local function apply_transparency(enabled)
    local bg = enabled and "NONE" or nil

    vim.api.nvim_set_hl(0, "Normal", { bg = 'none' })
    vim.api.nvim_set_hl(0, "NormalNC", { bg = 'none' })
    vim.api.nvim_set_hl(0, "NormalFloat", { bg = 'none' })
    vim.api.nvim_set_hl(0, "FloatBorder", { bg = 'none' })
    vim.api.nvim_set_hl(0, "SignColumn", { bg = 'none' })
    vim.api.nvim_set_hl(0, "EndOfBuffer", { bg = 'none' })
end

function M.reload_loom_theme()
    package.loaded["oracle.generated.theme"] = nil

    local ok, theme = pcall(require, "oracle.generated.theme")
    if not ok then
        vim.notify("Failed to load oracle.generated.theme", vim.log.levels.ERROR)
        return
    end

    if theme.colorscheme == "rose-pine" then
        require("rose-pine").setup({
            variant = theme.variant,
            disable_background = theme.transparent,
            styles = { italic = false },
        })
        vim.cmd.colorscheme("rose-pine")

    elseif theme.colorscheme == "gruvbox" then
        if theme.variant == "light" or theme.variant == "dark" then
            vim.o.background = theme.variant
        else
            vim.o.background = "dark"
        end
        vim.cmd.colorscheme("gruvbox")

    elseif theme.colorscheme == "catppuccin" then
        require("catppuccin").setup({
            flavour = theme.variant,
            transparent_background = theme.transparent,
            no_italic = true,
        })
        vim.cmd.colorscheme("catppuccin")

    elseif theme.colorscheme == "vague" then
        require("vague").setup({
            transparent = theme.transparent,
        })
        vim.cmd.colorscheme("vague")

    elseif theme.colorscheme == "dayfox" then
        vim.cmd.colorscheme("dayfox")

    elseif theme.colorscheme == "dawnfox" then
        vim.cmd.colorscheme("dawnfox")

    elseif theme.colorscheme == "oxocarbon" then
        vim.cmd.colorscheme("oxocarbon")

    elseif theme.colorscheme == "reddish" then
        vim.cmd.colorscheme("reddish")

    elseif theme.colorscheme == "tokyonight" then
        vim.cmd.colorscheme("tokyonight")

    elseif theme.colorscheme == "loom-custom" then
        if not theme.palette then
            vim.notify("loom-custom selected but no palette present", vim.log.levels.ERROR)
            return
        end

        require("mini.base16").setup({
            palette = theme.palette,
            use_cterm = true,
        })

        -- No :colorscheme call here. mini.base16 applies highlights directly.
    else
        vim.notify("Unknown Loom colorscheme: " .. tostring(theme.colorscheme), vim.log.levels.ERROR)
        return
    end

    vim.schedule(function()
        apply_transparency(theme.transparent)
    end)
end

vim.api.nvim_create_user_command("LoomReloadTheme", function()
    M.reload_loom_theme()
end, {})

vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
        local server = vim.v.servername
        if server and server:match("^/tmp/nvim%-loom%-.*%.sock$") then
            pcall(vim.fn.delete, server)
        end
    end,
})

return M
